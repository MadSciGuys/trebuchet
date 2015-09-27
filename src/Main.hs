{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Hasql.Postgres as HP
import qualified Hasql as H
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC
import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.Types as MySQL
import System.Directory
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

---- Servant API Layout Types ----
type TrebApi = JobTemplateAllH :<|> JobAllH :<|> UserH

type JobTemplateAllH =
  "job_template" :> "all"
    :> Get '[JSON] [JobTemplate]

type JobAllH =
  "job" :> "all"
    :> Get '[JSON] [Job]

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
        :<|> jobAllH
        :<|> userH
  where
    ---- Handlers ----
    jobTemplateAllH :: TrebServer JobTemplateAllH
    jobTemplateAllH = ask >>= return . trebEnvJobTemplates

    jobAllH :: TrebServer JobAllH
    jobAllH = todoHandler
    
    userH :: TrebServer UserH
    userH = getUser

    ---- Helpers ----
    todoHandler = lift $ left $ err501

drupalAuth :: (Text -> TrebServerBase a) -> Maybe Text -> TrebServerBase a
drupalAuth action cookies = do
  let sessionCookie = fmap (fmap snd . find ((== "SESS249b7ba79335e5fe3b5934ff07174a20") . fst) . parseCookies . encodeUtf8) cookies
  conn <- fromJust <$> trebEnvDrupalMySQLConn <$> ask
  usernames <- maybe
    (lift $ left $ err403 { errBody = encode $ ClientError CEMissingSessionCookie "Drupal session cookie is not found." })
    (liftIO . MySQL.query conn "SELECT name FROM atrium_users INNER JOIN atrium_sessions ON atrium_users.uid = atrium_sessions.uid WHERE atrium_sessions.sid = ?" . MySQL.Only)
    sessionCookie
  case usernames of
    [] ->
      lift $ left $ err403 { errBody = encode $ ClientError CEInvalidSessionCookie "Drupal session cookie is found but either invalid or expired." }
    [MySQL.Only username] -> do
      ret <- action username
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
    conn <- maybe 
      (lift $ left $ err500 { errBody = "Drupal MySQL connection is Nothing in environment." })
      return =<< reader trebEnvDrupalMySQLConn
    rs <- liftIO $
        MySQL.query
            conn
            "SELECT atrium_users.uid, atrium_users.name, atrium_realname.realname, atrium_users.mail FROM atrium_users INNER JOIN atrium_realname ON atrium_realname.uid = atrium_users.uid WHERE atrium_users.name = ?"
            (MySQL.Only username)
    case rs of
        [(drupalUid, drupalUsername, drupalRealname, drupalEmail)] ->
            return $ User drupalUid drupalUsername drupalRealname drupalEmail
        _ ->
            lift $ left $ err404 { errBody = encode $ ClientError CEUserNotFound "User with username not found." }
