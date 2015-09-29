{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Hasql.Postgres as HP
import qualified Hasql as H
import qualified Hasql.Backend as HB
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString.Char8 as BC
import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.Types as MySQL
import qualified Database.MySQL.Simple.QueryParams as MySQL
import qualified Database.MySQL.Simple.QueryResults as MySQL
import System.Directory
import Data.Word
import Data.Aeson
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Data.Maybe
import Data.Monoid
import System.FilePath
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.State.Class
import System.INotify
import Control.Monad.State.Lazy
import Data.Text (Text, pack)
import Control.Concurrent
import Servant
import Servant.Server
import Control.Exception
import System.IO.Error
import Data.List (find)
import Web.Cookie
import Data.Text.Encoding
import System.Environment (getArgs)
import System.Exit
import Text.Read (readEither)
import Data.Bool
import Data.Bits (xor)
import Treb.Combinators
import Treb.Config
import Treb.Types
import Data.Time.Clock
import Data.Functor.Identity

---- Servant API Layout Types ----
type TrebApi = JobTemplateAllH :<|> JobCreateH :<|> UserH

type JobTemplateAllH =
  "job_template" :> "all"
    :> Get '[JSON] [JobTemplate]

type JobAllH =
  "job" :> "all"
    :> Get '[JSON] [Job]

type JobCreateH =
  "job" :> "create"
    :> ReqBody '[JSON] JobConfig
    :> Header "Cookie" Text
    :> Post '[JSON] Job

type UserH =
  "user" :> Capture "username" Text
  :> Get '[JSON] User

type DemoAuthH =
  "current_username"
    :> Header "Cookie" Text
    :> Get '[JSON] Value

---- Other Servant Related Types ----
type TrebServerBase = ReaderT TrebEnv (EitherT ServantErr IO)
type TrebServer layout = ServerT layout TrebServerBase

---- Important Functions ----
main :: IO ()
main = do
  withTrebEnv $ \ env -> do
    bool
      runSettings
      (runTLS $ fromJust $ tlsSettings <$> (confSSLCertPath $ trebEnvConfig env) <*> (confSSLCertKeyPath $ trebEnvConfig env))
      (useSSL env)
      (trebWarpSettings env)
      (trebApp env)
      
useSSL :: TrebEnv -> Bool
useSSL env = isJust (confSSLCertPath $ trebEnvConfig env) && isJust (confSSLCertKeyPath $ trebEnvConfig env)

trebWarpSettings :: TrebEnv -> Settings
trebWarpSettings env =
  foldl (flip ($)) defaultSettings
    [ setBeforeMainLoop $ putStrLn "Trebuchet is ready."
    , setPort $ confPort $ trebEnvConfig env ]

trebApp :: TrebEnv -> Application
trebApp = serve trebApiProxy . flip enter trebServer . runReaderTNat

trebServer :: TrebServer TrebApi
trebServer = jobTemplateAllH
        :<|> jobCreateH
        :<|> userH
  where
    ---- Handlers ----
    jobTemplateAllH :: TrebServer JobTemplateAllH
    jobTemplateAllH = ask >>= return . trebEnvJobTemplates

    jobCreateH :: TrebServer JobCreateH
    jobCreateH jc = drupalAuth $ \user -> do
        now <- liftIO getCurrentTime
        jobId <- queryPG H.singleEx $
            [H.stmt|insert into job
                    ( owner_id
                    , template_id
                    , name
                    , status
                    , start_time
                    , end_time
                    , output_datablock_id
                    , failure_reason )
                    values (?, ?, ?, 'running', ?, NULL, NULL, NULL)
                    returning id |]
                      (userID user)
                      (jobTemplateId $ jobConfigTemplate jc)
                      (jobConfigName jc)
                      now
        return $ Job (runIdentity jobId) (userName user) jc now Nothing Nothing
    
    userH :: TrebServer UserH
    userH = getUser

    ---- Helpers ----
    -- todoHandler = lift $ left $ err501

drupalAuth :: (User -> TrebServerBase a) -> Maybe Text -> TrebServerBase a
drupalAuth action cookies = do
  let sessionCookie = fmap (fmap snd . find ((== "SESS249b7ba79335e5fe3b5934ff07174a20") . fst) . parseCookies . encodeUtf8) cookies
  conn <- fromJust <$> trebEnvDrupalMySQLConn <$> ask
  users <- maybe
    (lift $ left $ err403 { errBody = encode $ ClientError CEMissingSessionCookie "Drupal session cookie is not found." })
    (liftIO . MySQL.query conn "SELECT atrium_users.uid, atrium_users.name, atrium_realname.realname, atrium_users.mail FROM atrium_users INNER JOIN atrium_sessions ON atrium_users.uid = atrium_sessions.uid INNER JOIN atrium_realname ON atrium_users.uid = atrium_realname.uid WHERE atrium_sessions.sid = ?" . MySQL.Only)
    sessionCookie
  case users of
    [] ->
      lift $ left $ err403 { errBody = encode $ ClientError CEInvalidSessionCookie "Drupal session cookie is found but either invalid or expired." }
    [(uid, username, realname, email)] -> do
      ret <- action $ User uid username realname email
      return ret
    _ ->
      lift $ left err500 { errBody = "SQL query invalid. Returned list of usernames has more than one element." }

---- Helper Functions ----
setTrebEnvJobTemplates :: TrebEnv -> [JobTemplate] -> TrebEnv
setTrebEnvJobTemplates env jts = env { trebEnvJobTemplates = jts }

setTrebEnvJobTemplatesTVar :: TrebEnv -> TVar (Maybe [JobTemplate]) -> TrebEnv
setTrebEnvJobTemplatesTVar env jts = env { trebEnvJobTemplatesTVar = jts }

setTrebEnvUsername :: TrebEnv -> Maybe Text -> TrebEnv
setTrebEnvUsername env username = env { trebEnvUsername = username }

--getUsername :: TrebServerBase Text
--getUsername = fromJust <$> trebEnvUsername <$> get 

trebApiProxy :: Proxy TrebApi
trebApiProxy = Proxy

getUser :: Text -> TrebServerBase User
getUser username = do
    -- TODO: Change trebEnvDrupalMySQLConn to not be in Maybe.
    rs <- queryDrupal (MySQL.Only username)
        "SELECT atrium_users.uid, atrium_users.name, atrium_realname.realname, atrium_users.mail FROM atrium_users INNER JOIN atrium_realname ON atrium_realname.uid = atrium_users.uid WHERE atrium_users.name = ?"
    case rs of
        [(drupalUid, drupalUsername, drupalRealname, drupalEmail)] ->
            return $ User drupalUid drupalUsername drupalRealname drupalEmail
        _ ->
            serverError "Drupal MySQL connection is Nothing in environment."
            

serverError :: B.ByteString -> TrebServerBase a
serverError msg = lift $ left $ err500 { errBody = msg }

clientError :: ClientErrorCode -> Text -> TrebServerBase a
clientError ce msg =
  (\ ret -> lift $ left $ ret { errBody = encode $ ClientError ce msg }) $ case ce of
    CEMissingSessionCookie -> err403
    CEInvalidSessionCookie -> err403
    CEUserNotFound         -> err404
    _                      -> err400

--getJobs :: TrebServerBase [Job]
--getJobs = do
	--[(i, oi, ti, n, s, st, et, odi, fr)]
        --[(i :: Word64, b :: Word64)] <- queryPG H.listEx [H.stmt|select * from job|]
        --return []
	-- NOTE: jobTemplate in JobConfig is of type JobTemplate and not and id.
	--mapM (\(i, oi, ti, n, s, st, et, odi, fr) -> do
        --  Job i oi (JobConfig n ti ))
--getJobs :: TrebServerBase [Job]
--getJobs = do
--    pool <- reader trebEnvPostgresPool
--    (id, ownerId, startTime, endTime, status, failureReason) <- H.session pool $ do
--        H.singleEx [H.stmt|select from |]
--    conn <- maybe 
--      (lift $ left $ err500 { errBody = "Drupal MySQL connection is Nothing in environment." })
--      return =<< reader trebEnvDrupalMySQLConn
--    us <- liftIO $ MySQL.query "select name from atrium_users where uid = ?" (MySQL.Only ownerId)
--    username <- case us of
--        [MySQL.Only u] ->
--            return u
--        _ ->
--            lift $ left $ err500 { errBody = "User with username not found in Drupal." }
--    let stat = case status of
--        "running"  -> Nothing
--        "success"  -> Just $ Right endTime
--        "failed"   -> Just $ Left $ JobFailed failureReason endTime
--        "canceled" -> Just $ Left $ JobCanceled 
--    Job id ownerId jc startTime 

getDrupalMySQLConn :: TrebServerBase MySQL.Connection
getDrupalMySQLConn = do
    maybeConn <- reader trebEnvDrupalMySQLConn
    maybe 
      (lift $ left $ err500 { errBody = "Drupal MySQL connection is Nothing in environment." })
      return
      maybeConn

getPgPool :: TrebServerBase (H.Pool HP.Postgres)
getPgPool = reader trebEnvPgPool

queryDrupal :: (MySQL.QueryParams q, MySQL.QueryResults r) => q -> MySQL.Query -> TrebServerBase [r]
queryDrupal params query = do
    conn <- getDrupalMySQLConn
    liftIO $ MySQL.query conn query params

--queryPGTx :: H.Tx HP.Postgres s r -> TrebServerBase r
--queryPGTx tx' = do
--    pool <- getPgPool
--    res <- liftIO $ H.session pool (H.tx Nothing tx' :: H.Session HP.Postgres IO r)
--    either
--        (lift . left . const err500 { errBody = "Postgres failure." }) -- TODO: errBody = B.toStrict $ encodeUtf8 $ pack $ show err
--        return
--    	res

queryPG :: forall r. (forall s. H.Ex HP.Postgres s r) -> H.Stmt HP.Postgres -> TrebServerBase r
queryPG ex stmt = do
    pool <- getPgPool
    res <- liftIO $ H.session pool (H.tx Nothing (ex stmt :: H.Tx HP.Postgres s r) :: H.Session HP.Postgres IO r)
    either
        (lift . left . const err500 { errBody = "Postgres failure." }) -- TODO: errBody = B.toStrict $ encodeUtf8 $ pack $ show err
        return
    	res
