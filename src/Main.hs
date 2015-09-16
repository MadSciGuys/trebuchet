{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Treb.ExtTypes
import Treb.ExtJSON
import Treb.DB.Schema (getPool)
import Treb.DB.Statements

import qualified Hasql.Postgres as HP
import qualified Hasql as H
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import System.Directory (getDirectoryContents, doesFileExist)
import Data.Aeson
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
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
import Data.Text (Text)
import Control.Concurrent
import Servant
import Servant.Server

---- Servant API Layout Types ----
type TrebApi = JobTemplateAllH :<|> JobAllH

type JobTemplateAllH =
  "job_template" :> "all"
    :> Get '[JSON] [JobTemplate]

type JobAllH =
  "job" :> "all"
    :> Get '[JSON] [Job]

---- Other Servant Related Types ----
data TrebEnv = TrebEnv
  { trebEnvJobTemplates :: [JobTemplate]
    -- ^ This is the current set of acknowledged job templates.
  , trebEnvJobTemplatesTVar :: TVar (Maybe [JobTemplate])
    -- ^ This TVar is written to upon inotify events in the job templates
    -- directory.
  }

type TrebServer layout = ServerT layout (StateT TrebEnv (EitherT ServantErr IO))

---- Important Functions ----
main :: IO ()
main = do
  ---- Initialize ----
  -- Read in job templates from JSON files in job_templates directory.
  jobTemplates <- getJobTemplates "job_templates"

  -- Create a pool of connections to Postgres
  pool <- getPool

  -- Beget the initial Trebuchet environment state via IO
  env <- getEnv

  ---- Run ----
  run 3000 $ serve trebApiProxy $ server env

server :: TrebEnv -> Server TrebApi
server = flip enter trebServer . evalStateTLNat

trebServer :: TrebServer TrebApi
trebServer = wrapHandler jobTemplateAllH
        :<|> wrapHandler jobAllH
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

    ---- Helpers ----
    todoHandler = lift $ left $ err501

getEnv :: IO TrebEnv
getEnv = do
  -- Create TVar for updating the job templates available to HTTP request handlers
  jobTemplatesTVar <- newTVarIO Nothing

  -- Begin watching job_templates directory and automatically update the internal job templates accordingly
  inotify <- initINotify
  addWatch inotify [Create, Delete, Modify, MoveIn, MoveOut] "job_templates" $ \ _ -> do
    jobTemplates <- getJobTemplates "job_templates"
    atomically $ swapTVar jobTemplatesTVar (Just jobTemplates)
    putStrLn "Job Templates Updated."
  
  -- Get the initial job templates
  jobTemplates <- getJobTemplates "job_templates"

  -- Construct the Trebuchet environment
  return $ TrebEnv
    { trebEnvJobTemplates = jobTemplates
    , trebEnvJobTemplatesTVar = jobTemplatesTVar }

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
  templateFiles' <- getDirectoryContents templateDir
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

---- Helper Functions ----
setTrebEnvJobTemplates :: TrebEnv -> [JobTemplate] -> TrebEnv
setTrebEnvJobTemplates env jts = env { trebEnvJobTemplates = jts }

setTrebEnvJobTemplatesTVar :: TrebEnv -> TVar (Maybe [JobTemplate]) -> TrebEnv
setTrebEnvJobTemplatesTVar env jts = env { trebEnvJobTemplatesTVar = jts }

trebApiProxy :: Proxy TrebApi
trebApiProxy = Proxy
