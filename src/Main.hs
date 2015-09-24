{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Treb.ExtTypes
import Treb.ExtJSON

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

---- Servant API Layout Types ----
type TrebApi = JobTemplateAllH :<|> JobAllH :<|> DemoAuthH

type JobTemplateAllH =
  "job_template" :> "all"
    :> Get '[JSON] [JobTemplate]

type JobAllH =
  "job" :> "all"
    :> Get '[JSON] [Job]

type DemoAuthH =
  "current_username"
    :> Header "Cookie" Text
    :> Get '[JSON] Value

---- Other Servant Related Types ----
data TrebEnv = TrebEnv
  { trebEnvJobTemplates :: [JobTemplate]
    -- ^ This is the current set of acknowledged job templates.
  , trebEnvJobTemplatesTVar :: TVar (Maybe [JobTemplate])
    -- ^ This TVar is written to upon inotify events in the job templates
    -- directory.
  , trebEnvDrupalMySQLConn :: Maybe MySQL.Connection
    -- ^ This is intended for authentication.
  , trebEnvUsername :: Maybe Text -- ^ Temporary. To be replaced by trebEnvUser
  , trebEnvConfig :: TrebConfig
  }

data TrebConfig = TrebConfig
  { confDebugMode      :: Bool
  , confPort           :: Int
  , confJobTemplateDir :: String
  , confSSLCertPath    :: Maybe String
  , confSSLCertKeyPath :: Maybe String
  , confOAHost         :: Maybe String
  , confOAPort         :: Maybe String
  , confOADatabase     :: Maybe String
  , confOAUsername     :: Maybe String
  , confOAPassword     :: Maybe String
  , confOADomain       :: Maybe String
  , confPGHost         :: Maybe String
  , confPGPort         :: Maybe String
  , confPGUsername     :: Maybe String
  , confPGPassword     :: Maybe String
  , confPGDatabase     :: Maybe String
  , confPGPoolMax      :: Maybe String
  , confPGConnLifetime :: Maybe String }

defaultTrebConfig = TrebConfig
    { confDebugMode      = False
    , confPort           = 3000
    , confJobTemplateDir = "job_templates"
    , confSSLCertPath    = Nothing
    , confSSLCertKeyPath = Nothing
    , confOAHost         = Nothing
    , confOAPort         = Nothing
    , confOADatabase     = Nothing
    , confOAUsername     = Nothing
    , confOAPassword     = Nothing
    , confOADomain       = Nothing
    , confPGHost         = Nothing
    , confPGPort         = Nothing
    , confPGUsername     = Nothing
    , confPGPassword     = Nothing
    , confPGDatabase     = Nothing
    , confPGPoolMax      = Nothing
    , confPGConnLifetime = Nothing }

type TrebServerBase = StateT TrebEnv (EitherT ServantErr IO)
type TrebServer layout = ServerT layout TrebServerBase

---- Important Functions ----
withTrebEnv :: (TrebEnv -> IO ()) -> IO ()
withTrebEnv f = eitherT (putStrLn . ("ERROR: " <>)) f getEnv

main :: IO ()
main = do
  ---- Initialize ----
  -- Beget the initial Trebuchet environment state via IO
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
trebApp = serve trebApiProxy . flip enter trebServer . evalStateTLNat

trebServer :: TrebServer TrebApi
trebServer = wrapHandler jobTemplateAllH
        :<|> wrapHandler jobAllH
        :<|> (wrapHandler . demoAuthH)
  where
    -- | Begin all HTTP requests with a common pre-handler action and follow
    -- all HTTP requests with a common post-handler action.
    wrapHandler :: (MonadIO m, MonadState TrebEnv m) => m a -> m a
    wrapHandler action = do
      get >>= liftIO . preHandlerIO >>= put
      ret <- action
      get >>= liftIO . postHandlerIO >>= put
      return ret

    preHandlerIO :: TrebEnv -> IO TrebEnv
    preHandlerIO env = do
      ---- Update Job Templates ----
      -- Read the job templates TVar
      jobTemplates <- readTVarIO (trebEnvJobTemplatesTVar env)
      -- If it contains a fresh set of job templates, update the environment accordingly.
      return $ maybe env (setTrebEnvJobTemplates env) jobTemplates

    postHandlerIO :: TrebEnv -> IO TrebEnv
    postHandlerIO =
      ---- Do Nothing (for now) ----
      return

    ---- Handlers ----
    jobTemplateAllH :: TrebServer JobTemplateAllH
    jobTemplateAllH = get >>= return . trebEnvJobTemplates

    jobAllH :: TrebServer JobAllH
    jobAllH = todoHandler
    -- ^ It is going to look something like this:
    --     jobs <- H.session (initDbPool st) getJobs
    
    demoAuthH :: TrebServer DemoAuthH
    demoAuthH = drupalAuth $ do
      username <- getUsername
      return $ object [ "username" .= username ]

    ---- Helpers ----
    todoHandler = lift $ left $ err501

ifDebugMode :: Monad m => TrebConfig -> m a -> m (Maybe a)
ifDebugMode conf action = bool (return Nothing) (action >>= return . Just) (confDebugMode conf)

unlessDebugMode :: Monad m => TrebConfig -> m a -> m (Maybe a)
unlessDebugMode conf action = bool (action >>= return . Just) (return Nothing) (confDebugMode conf)

trebEnvGetJobTemplate :: TrebEnv -> JobTemplateId -> Maybe JobTemplate
trebEnvGetJobTemplate = flip M.lookup . M.fromList . map (\jt -> (jobTemplateId jt, jt)) . trebEnvJobTemplates

trebEnvGetParamType :: TrebEnv -> JobTemplateId -> Text -> Maybe JobTemplateParameterType
trebEnvGetParamType env jtId paramKeyName =
  M.lookup paramKeyName $ fromMaybe M.empty $ do
    jt <- trebEnvGetJobTemplate env jtId
    return $ M.fromList $ map (\p -> (jobTemplateParameterKeyName p, jobTemplateParameterType p)) $ jobTemplateParameters jt

getJobTemplates :: FilePath -> IO [JobTemplate]
getJobTemplates templateDir = do
  -- Get a list of job template file names
  templateFiles' <- getDirectoryContents templateDir `catch` \e ->
    if isDoesNotExistError e then do
      fullTemplateDir <- makeAbsolute templateDir
      putStrLn $ "ERROR: Job template specification directory '" ++ fullTemplateDir ++ "' does not exist."
      createDirectoryIfMissing False fullTemplateDir
      putStrLn $ "Made new directory '" ++ fullTemplateDir ++ "'."
      return []
    else
      throw e
  templateFiles <- filterM doesFileExist $ map (templateDir </>) templateFiles'
  -- Get a list of decoded job templates
  jobTemplates <- mapM (fmap eitherDecode . B.readFile) templateFiles
  -- Print an error on each failure to decode a job template.
  let parseResults = [ either (Left . ((,) f)) Right t | (f, t) <- zip templateFiles jobTemplates ]
  results <- mapM (either printError (return . Just)) parseResults
  -- Return only successfully parsed job templates
  return $ map fromJust $ filter isJust results
  where
    printError (file, error) = do
      putStrLn $ "ERROR: Failed to parse job template JSON: " <> file <> "\n\n" <> error
      return Nothing

drupalAuth :: TrebServerBase a -> Maybe Text -> TrebServerBase a
drupalAuth action cookies = do
  let sessionCookie = fmap (fmap snd . find ((== "SESS249b7ba79335e5fe3b5934ff07174a20") . fst) . parseCookies . encodeUtf8) cookies
  conn <- getDrupalMySQLConn
  usernames <- maybe
    (lift $ left $ err403 { errBody = encode $ ClientError CEMissingSessionCookie "Drupal session cookie is not found." })
    (liftIO . MySQL.query conn "SELECT name FROM atrium_users INNER JOIN atrium_sessions ON atrium_users.uid = atrium_sessions.uid WHERE atrium_sessions.sid = ?" . MySQL.Only)
    sessionCookie
  case usernames of
    [] ->
      lift $ left $ err403 { errBody = encode $ ClientError CEInvalidSessionCookie "Drupal session cookie is found but either invalid or expired." }
    [MySQL.Only username] -> do
      modify (flip setTrebEnvUsername $ Just username)
      ret <- action
      modify (flip setTrebEnvUsername Nothing)
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

getDrupalMySQLConn :: TrebServerBase MySQL.Connection
getDrupalMySQLConn = fromJust <$> trebEnvDrupalMySQLConn <$> get 

getUsername :: TrebServerBase Text
getUsername = fromJust <$> trebEnvUsername <$> get 

trebApiProxy :: Proxy TrebApi
trebApiProxy = Proxy
