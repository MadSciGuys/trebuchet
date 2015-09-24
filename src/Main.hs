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

leftIf :: Monad m => Bool -> l -> EitherT l m ()
leftIf b l = bool (return ()) (left l) b

ifDebugMode :: Monad m => TrebConfig -> m a -> m (Maybe a)
ifDebugMode conf action = bool (return Nothing) (action >>= return . Just) (confDebugMode conf)

unlessDebugMode :: Monad m => TrebConfig -> m a -> m (Maybe a)
unlessDebugMode conf action = bool (action >>= return . Just) (return Nothing) (confDebugMode conf)

-- connSettings :: HP.Settings
-- connSettings = HP.ParamSettings "10.37.49.24" 5432 "mswan" "mswan" "trebuchet"
-- 
-- poolSettings :: Maybe H.PoolSettings
-- poolSettings = H.poolSettings 6 30

getPool :: TrebConfig -> EitherT String IO (H.Pool HP.Postgres)
getPool conf = do
  mapM_ (\(attr, msg) ->
    leftIf
      (isNothing $ attr conf)
      $ msg <> " for PostgreSQL database not given.")
    [ (confPGHost,         "Host")
    , (confPGPort,         "Port")
    , (confPGUsername,     "Username")
    , (confPGPassword,     "Password")
    , (confPGDatabase,     "Database name")
    , (confPGPoolMax,      "Maximum pool size")
    , (confPGConnLifetime, "Connection duration") ]

  pgPort         <- hoistEither $ readEither $ fromJust $ confPGPort conf
  pgPoolMax      <- hoistEither $ readEither $ fromJust $ confPGPoolMax conf
  pgConnLifetime <- hoistEither $ readEither $ fromJust $ confPGConnLifetime conf

  maybe
    (left "Invalid PostgreSQL pool settings.")
    (liftIO . uncurry H.acquirePool)
    $ (,) <$> (HP.ParamSettings <$> fmap BC.pack (confPGHost conf)
                                <*> pure pgPort
                                <*> fmap BC.pack (confPGUsername conf)
                                <*> fmap BC.pack (confPGPassword conf)
                                <*> fmap BC.pack (confPGDatabase conf))
          <*> (fromMaybe Nothing $ H.poolSettings <$> pure pgPoolMax
                                                  <*> pure pgConnLifetime)

getEnv :: EitherT String IO TrebEnv
getEnv = do
  -- Generate configuration from command line arguments
  conf <- processArgs defaultTrebConfig =<< liftIO getArgs

  -- Create a pool of connections to PostgreSQL
  pool <- getPool conf

  -- Check that SSL-related command line arguments are well formed
  leftIf
    (isJust (confSSLCertPath conf) `xor` isJust (confSSLCertKeyPath conf))
    $ "SSL requires both -c/--ssl-certificate and -k/--ssl-certificate-key to be set."
    
  -- Check that the job template directory exists
  let jobTemplateDir = confJobTemplateDir conf

  cwd <- liftIO getCurrentDirectory
  jobTemplateDirExists <- liftIO $ doesFileExist jobTemplateDir

  leftIf
    jobTemplateDirExists
    $ "Job template directory '" <> (cwd </> jobTemplateDir) <> "' not found."

  -- Create TVar for updating the job templates available to HTTP request handlers
  jobTemplatesTVar <- liftIO $ newTVarIO Nothing

  -- Begin watching job_templates directory and automatically update the internal job templates accordingly
  liftIO $ do
    putStrLn "Initializing event watchers for job templates directory."
  
    inotify <- initINotify
    addWatch inotify [Create, Delete, Modify, MoveIn, MoveOut] jobTemplateDir $ \ _ ->
      getJobTemplates jobTemplateDir
        >>= atomically . swapTVar jobTemplatesTVar . Just
        >> putStrLn "Job Templates Updated."

    putStrLn "> Done."

  -- Get the initial job templates
  jobTemplates <- liftIO $ putStrLn "Parsing job templates."
                        *> getJobTemplates jobTemplateDir
                        <* putStrLn "> Done."

  -- Connect to the Drupal/OpenAtrium MySQL database for authentication and authorization
  drupalMySQLConn <- unlessDebugMode conf $ do
    mapM_ (\(attr, msg) ->
      leftIf
        (isNothing $ attr conf)
        $ msg <> " for OpenAtrium database not given.")
      [ (confOAHost,     "Host")
      , (confOAPort,     "Port")
      , (confOADatabase, "Database name")
      , (confOAUsername, "Username")
      , (confOAPassword, "Password") ]

    liftIO $ putStrLn "Connecting to Drupal/OpenAtrium MySQL database."

    oaPort <- hoistEither $ readEither $ fromJust $ confOAPort conf
    ret <- liftIO $ MySQL.connect $
       MySQL.defaultConnectInfo
         { MySQL.connectHost     = fromJust $ confOAHost conf
         , MySQL.connectPort     = oaPort
         , MySQL.connectDatabase = fromJust $ confOADatabase conf
         , MySQL.connectUser     = fromJust $ confOAUsername conf
         , MySQL.connectPassword = fromJust $ confOAPassword conf }

    liftIO $ putStrLn "> Done."
    return ret

  -- Construct the Trebuchet environment
  return $ TrebEnv
    { trebEnvJobTemplates     = jobTemplates
    , trebEnvJobTemplatesTVar = jobTemplatesTVar
    , trebEnvDrupalMySQLConn  = drupalMySQLConn
    , trebEnvUsername         = Nothing
    , trebEnvConfig           = conf }
  where
    catchAny :: (SomeException -> IO a) -> IO a -> IO a
    catchAny = flip catch

    processArgs :: TrebConfig -> [String] -> EitherT String IO TrebConfig
    processArgs conf []                                                      = right conf
    processArgs conf (x  :xs) | x == "-d" || x == "--debug"                  = processArgs (conf { confDebugMode      = True })   xs
    processArgs conf (x:y:xs) | x == "-c" || x == "--ssl-certificate"        = processArgs (conf { confSSLCertPath    = Just y }) xs
    processArgs conf (x:y:xs) | x == "-k" || x == "--ssl-certificate-key"    = processArgs (conf { confSSLCertKeyPath = Just y }) xs
    processArgs conf (x:y:xs) | x == "-t" || x == "--job-template-directory" = processArgs (conf { confJobTemplateDir = y })      xs
    processArgs conf (x:y:xs) | x == "-p" || x == "--port"                   = either
                                                                                 left
                                                                                 (\p -> processArgs (conf { confPort  = p })      xs)
                                                                                 (readEither y)
    processArgs conf (x:y:xs) | x == "-H" || x == "--oa-host"                = processArgs (conf { confOAHost         = Just y }) xs
    processArgs conf (x:y:xs) | x == "-P" || x == "--oa-port"                = processArgs (conf { confOAPort         = Just y }) xs
    processArgs conf (x:y:xs) | x == "-D" || x == "--oa-database"            = processArgs (conf { confOADatabase     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-U" || x == "--oa-username"            = processArgs (conf { confOAUsername     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-P" || x == "--oa-password"            = processArgs (conf { confOAPassword     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-C" || x == "--oa-cookie-domain"       = processArgs (conf { confOADomain       = Just y }) xs
    processArgs conf (x:y:xs) | x == "-h" || x == "--pg-host"                = processArgs (conf { confPGHost         = Just y }) xs
    processArgs conf (x:y:xs) | x == "-b" || x == "--pg-port"                = processArgs (conf { confPGPort         = Just y }) xs
    processArgs conf (x:y:xs) | x == "-u" || x == "--pg-username"            = processArgs (conf { confPGUsername     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-w" || x == "--pg-password"            = processArgs (conf { confPGPassword     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-s" || x == "--pg-database"            = processArgs (conf { confPGDatabase     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-m" || x == "--pg-pool-max"            = processArgs (conf { confPGPoolMax      = Just y }) xs
    processArgs conf (x:y:xs) | x == "-l" || x == "--pg-conn-lifetime"       = processArgs (conf { confPGConnLifetime = Just y }) xs
    processArgs conf (x:_)                                                   = left $ "ERROR: Invalid command-line argument \'" <> x <> "\'."

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
