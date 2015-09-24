module Treb.Config (withEnv) where

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
import Treb.Types

withTrebEnv :: (TrebEnv -> IO ()) -> IO ()
withTrebEnv f = eitherT (putStrLn . ("ERROR: " ++)) f getEnv

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
    $ "Job template directory '" ++ (cwd </> jobTemplateDir) ++ "' not found."

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
        $ msg ++ " for OpenAtrium database not given.")
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
    processArgs conf (x:_)                                                   = left $ "ERROR: Invalid command-line argument \'" ++ x ++ "\'."

getPool :: TrebConfig -> EitherT String IO (H.Pool HP.Postgres)
getPool conf = do
  mapM_ (\(attr, msg) ->
    leftIf
      (isNothing $ attr conf)
      $ msg ++ " for PostgreSQL database not given.")
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
      putStrLn $ "ERROR: Failed to parse job template JSON: " ++ file ++ "\n\n" ++ error
      return Nothing

