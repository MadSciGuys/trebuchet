{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Treb.ExtTypes
import Treb.ExtJSON

import System.Directory (getDirectoryContents, doesFileExist)
import Data.Aeson
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Monoid
import System.FilePath
import Control.Monad

import Servant

type TrebApi =
  "job_template" :> "all"
    :> Get '[JSON] [JobTemplate]

data InitState = InitState
  { initJobTemplates :: [JobTemplate] }

server :: InitState -> Server TrebApi
server st =
  jobTemplateAllH
  where
    jobTemplateAllH = return $ initJobTemplates st

trebApi :: Proxy TrebApi
trebApi = Proxy

app :: InitState -> Application
app = serve trebApi . server

main :: IO ()
main = do
  -- Initialize --
  -- Read in job templates from JSON files in job_templates directory.
  jobTemplates <- getJobTemplates "job_templates"

  -- Run the server --
  run 3000 (app InitState { initJobTemplates = jobTemplates
                          })

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
