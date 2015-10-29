{-# LANGUAGE RankNTypes, ImpredicativeTypes, LiberalTypeSynonyms #-}
module Treb.Config (withTrebEnv) where

import qualified Hasql as H
import qualified Hasql.Postgres as HP
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC
import qualified Database.MySQL.Simple as MySQL
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.STM
import Data.Aeson
import Data.Bool
import Data.Bits (xor)
import Data.Either (either)
import Data.Maybe
import Network.URI
import System.FilePath
import System.INotify
import System.Directory
import System.Random
import System.Environment (getArgs)
import System.IO.Error
import Text.Read (readEither)
import Treb.Combinators
import Treb.JSON ()
import Treb.Types
import Treb.Routes.Types

-- Exported Functions --

-- | Construct a Trebuchet environment and pass it into a function in IO. This
-- environment includes database connections and similar.
withTrebEnv :: (TrebEnv -> IO ()) -> IO ()
withTrebEnv f = runExceptT getEnv >>= either (putStrLn . ("ERROR: " ++)) f

-- Hidden Functions --

getEnv :: ExceptT String IO TrebEnv
getEnv = do
  -- Generate configuration from command line arguments
  conf <- processArgs defaultTrebConfig =<< liftIO getArgs

  -- Create a pool of connections to PostgreSQL
  pgPool <- getPool conf

  -- Check that SSL-related command line arguments are well formed
  exceptIf
    (isJust (confSSLCertPath conf) `xor` isJust (confSSLCertKeyPath conf))
    $ "SSL requires both -c/--ssl-certificate and -k/--ssl-certificate-key to be set."
    
  -- Check that the job template directory exists
  let jobTemplateDir = confJobTemplateDir conf

  cwd <- liftIO getCurrentDirectory
  jobTemplateDirExists <- liftIO $ doesFileExist jobTemplateDir

  exceptIf
    jobTemplateDirExists
    $ "Job template directory '" ++ (cwd </> jobTemplateDir) ++ "' not found."

  -- Create TVar for updating the job templates available to HTTP request handlers
  jobTemplates <- liftIO $ newTVarIO []

  -- Begin watching job_templates directory and automatically update the internal job templates accordingly
  liftIO $ do
    let updateJobTemplates = getJobTemplates jobTemplateDir >>= atomically . writeTVar jobTemplates

    putStrLn "Initializing event watchers for job templates directory."
    inotify <- initINotify
    addWatch inotify [Create, Delete, Modify, MoveIn, MoveOut] jobTemplateDir $
      const $ updateJobTemplates
           >> putStrLn "Job Templates Updated."
    putStrLn "> Done."

    putStrLn "Settings initial Job Templates."
    updateJobTemplates
    putStrLn "> Done."

  -- Connect to the Drupal/OpenAtrium MySQL database for authentication and authorization
  drupalMySQLConn <- unlessDebugMode conf $ do
    mapM_ (\(attr, msg) ->
      exceptIf
        (isNothing $ attr conf)
        $ msg ++ " for OpenAtrium database not given.")
      [ (confOAHost,     "Host")
      , (confOAPort,     "Port")
      , (confOADatabase, "Database name")
      , (confOAUsername, "Username")
      , (confOAPassword, "Password") ] :: ExceptT String IO ()

    liftIO $ putStrLn "Connecting to Drupal/OpenAtrium MySQL database."

    oaPort <- either throwE return $ readEither $ fromJust $ confOAPort conf
    ret <- liftIO $ MySQL.connect $
       MySQL.defaultConnectInfo
         { MySQL.connectHost     = fromJust $ confOAHost conf
         , MySQL.connectPort     = oaPort
         , MySQL.connectDatabase = fromJust $ confOADatabase conf
         , MySQL.connectUser     = fromJust $ confOAUsername conf
         , MySQL.connectPassword = fromJust $ confOAPassword conf }

    liftIO $ putStrLn "> Done."
    return ret

  activeUploads <- liftIO $ newTVarIO M.empty
  uploadIdGen <- liftIO $ newTVarIO =<< getStdGen

  maybe (throwE "No --base-uri specified.")
        (bool (throwE "Invalid --base-uri given.")
              (return ()))
        (isURI <$> confBaseURI conf)

  baseURI <- fromMaybe
         (throwE "Failed to parse value given to --base-uri.")
         (confBaseURI conf >>= fmap pure . parseURI)

  -- Construct the Trebuchet environment
  return TrebEnv
    { trebEnvJobTemplates    = jobTemplates
    , trebEnvDrupalMySQLConn = drupalMySQLConn
    , trebEnvUsername        = Nothing
    , trebEnvConfig          = conf
    , trebEnvPgPool          = pgPool
    , trebEnvActiveUploads   = activeUploads
    , trebEnvCurrentUser     = Nothing
    , trebEnvUploadIdGen     = uploadIdGen
    , trebEnvBaseURI         = baseURI }

processArgs :: TrebConfig -> [String] -> ExceptT String IO TrebConfig
processArgs conf []                                                      = pure conf
processArgs conf (x  :xs) | x == "-d" || x == "--debug"                  = processArgs (conf { confDebugMode      = True })   xs
processArgs conf (x:y:xs) | x == "-c" || x == "--ssl-certificate"        = processArgs (conf { confSSLCertPath    = Just y }) xs
processArgs conf (x:y:xs) | x == "-k" || x == "--ssl-certificate-key"    = processArgs (conf { confSSLCertKeyPath = Just y }) xs
processArgs conf (x:y:xs) | x == "-t" || x == "--job-template-directory" = processArgs (conf { confJobTemplateDir = y })      xs
processArgs conf (x:y:xs) | x == "-p" || x == "--port"                   = either
                                                                             throwE
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
processArgs conf (x:y:xs) | x == "-B" || x == "--base-uri"               = processArgs (conf { confBaseURI        = Just y }) xs
processArgs conf (x:_)                                                   = throwE $ "ERROR: Invalid command-line argument \'" ++ x ++ "\'."

getPool :: TrebConfig -> ExceptT String IO (H.Pool HP.Postgres)
getPool conf = do
  mapM_ (\(attr, msg) ->
    exceptIf
      (isNothing $ attr conf)
      $ msg ++ " for PostgreSQL database not given.")
    [ (confPGHost,         "Host")
    , (confPGPort,         "Port")
    , (confPGUsername,     "Username")
    , (confPGPassword,     "Password")
    , (confPGDatabase,     "Database name")
    , (confPGPoolMax,      "Maximum pool size")
    , (confPGConnLifetime, "Connection duration") ]

  pgPort         <- either throwE return $ readEither $ fromJust $ confPGPort conf
  pgPoolMax      <- either throwE return $ readEither $ fromJust $ confPGPoolMax conf
  pgConnLifetime <- either throwE return $ readEither $ fromJust $ confPGConnLifetime conf

  maybe
    (throwE "Invalid PostgreSQL pool settings.")
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
    , confPGConnLifetime = Nothing
    , confBaseURI        = Nothing }

ifDebugMode :: Monad m => TrebConfig -> m a -> m (Maybe a)
ifDebugMode conf action = bool (return Nothing) (action >>= return . Just) (confDebugMode conf)

unlessDebugMode :: Monad m => TrebConfig -> m a -> m (Maybe a)
unlessDebugMode conf action = bool (action >>= return . Just) (return Nothing) (confDebugMode conf)
