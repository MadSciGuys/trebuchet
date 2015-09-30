{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}
module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.QueryParams as MySQL
import qualified Database.MySQL.Simple.QueryResults as MySQL
import qualified Hasql as H
import qualified Hasql.Postgres as HP
import Data.Word
import Data.Aeson
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Data.Maybe
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Data.Text (Text)
import Servant
import Data.List (find)
import Web.Cookie
import Data.Text.Encoding
import Data.Bool
import Treb.Config
import Treb.Types
import Treb.Routes
import Treb.Routes.Helpers
import Data.Time.Clock
import Data.Functor.Identity
import Treb.Routes
import Treb.Routes.Types

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
trebServer = dataBlockCreateH
    ---- Handlers ----
    --jobTemplateAllH :: TrebServer JobTemplateAllH
    --jobTemplateAllH = ask >>= liftIO . readTVar . trebEnvJobTemplates

    --jobCreateH :: TrebServer JobCreateH
    --jobCreateH jc = drupalAuth $ \user -> do
    --    now <- liftIO getCurrentTime
    --    Identity jobId <- queryPG H.singleEx $
    --        [H.stmt|insert into job
    --                ( owner_id
    --                , template_id
    --                , name
    --                , status
    --                , start_time
    --                , end_time
    --                , output_datablock_id
    --                , failure_reason )
    --                values (?, ?, ?, 'running', ?, NULL, NULL, NULL)
    --                returning id |]
    --                  (userID user)
    --                  (jobConfigTemplateId jc)
    --                  (jobConfigName jc)
    --                  now
    --    mapM_ (uncurry $ jobArgCreate jobId . Just) (M.toList $ jobConfigArgs jc)

    --    return $ Job jobId (userName user) jc now Nothing Nothing
    --    where
    --      insertBool     :: Word64 -> Maybe Text -> Bool -> H.Stmt HP.Postgres
    --      insertBool     = [H.stmt|insert into job_argument (job_id, name, type, value_bool) values (?,?,'bool',?)|]
    --      insertString   :: Word64 -> Maybe Text -> Text -> H.Stmt HP.Postgres
    --      insertString   = [H.stmt|insert into job_argument (job_id, name, type, value_string) values (?,?,'string',?)|]
    --      insertInt      :: Word64 -> Maybe Text -> Word64 -> H.Stmt HP.Postgres
    --      insertInt      = [H.stmt|insert into job_argument (job_id, name, type, value_int) values (?,?,'int',?)|]
    --      insertReal     :: Word64 -> Maybe Text -> Double -> H.Stmt HP.Postgres
    --      insertReal     = [H.stmt|insert into job_argument (job_id, name, type, value_real) values (?,?,'real',?)|]
    --      insertDateTime :: Word64 -> Maybe Text -> UTCTime -> H.Stmt HP.Postgres
    --      insertDateTime = [H.stmt|insert into job_argument (job_id, name, type, value_datetime) values (?,?,'datetime',?)|]
    --      insertVector   :: Word64 -> Maybe Text -> V.Vector Word64 -> H.Stmt HP.Postgres
    --      insertVector   = [H.stmt|insert into job_argument (job_id, name, type, value_vector) values (?,?,'vector',?)|]

    --      q :: H.Stmt HP.Postgres -> TrebServerBase Word64
    --      q = fmap runIdentity . queryPG H.singleEx

    --      jobArgCreate :: Word64 -> Maybe Text -> JobArg -> TrebServerBase Word64
    --      jobArgCreate jobId name arg =
    --        case arg of
    --          BoolArg b         -> q $ insertBool   jobId name b
    --          IntArg i          -> q $ insertInt    jobId name i
    --          RealArg r         -> q $ insertReal   jobId name r
    --          StringArg s       -> q $ insertString jobId name s
    --          EnumArg s         -> q $ insertString jobId name s
    --          RegexArg s        -> q $ insertString jobId name s
    --          DataBlockTagArg s -> q $ insertString jobId name s
    --          VectorArg v       -> do
    --            childArgIds <- V.mapM (jobArgCreate jobId Nothing) v
    --            runIdentity <$> queryPG H.singleEx (insertVector jobId name childArgIds)
    --          
    --userH :: TrebServer UserH
    --userH = getUser

---- Helper Functions ----
-- setTrebEnvJobTemplates :: TrebEnv -> [JobTemplate] -> TrebEnv
-- setTrebEnvJobTemplates env jts = env { trebEnvJobTemplates = jts }
-- 
-- setTrebEnvJobTemplatesTVar :: TrebEnv -> TVar (Maybe [JobTemplate]) -> TrebEnv
-- setTrebEnvJobTemplatesTVar env jts = env { trebEnvJobTemplatesTVar = jts }
-- 
-- setTrebEnvUsername :: TrebEnv -> Maybe Text -> TrebEnv
-- setTrebEnvUsername env username = env { trebEnvUsername = username }

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

--queryPGTx :: H.Tx HP.Postgres s r -> TrebServerBase r
--queryPGTx tx' = do
--    pool <- getPgPool
--    res <- liftIO $ H.session pool (H.tx Nothing tx' :: H.Session HP.Postgres IO r)
--    either
--        (lift . left . const err500 { errBody = "Postgres failure." }) -- TODO: errBody = B.toStrict $ encodeUtf8 $ pack $ show err
--        return
--    	res

