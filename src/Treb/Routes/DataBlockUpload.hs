{-|
Module:      Treb.Routes.DataBlockUpload
Description: Trebuchet DataBlockUpload route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, TypeFamilies,
             ExistentialQuantification, FlexibleInstances, ScopedTypeVariables,
             TypeSynonymInstances #-}

module Treb.Routes.DataBlockUpload
    ( DataBlockUploadH
    , dataBlockUploadH
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Either ()
import Data.Aeson (Value, ToJSON, toJSON, (.=), object)
import Treb.Routes.Helpers
import Treb.Routes.Types
import Treb.Types
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar ()
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import ProtoDB.Types
import ProtoDB.Parser
import Data.Text.Encoding (decodeUtf8)
import Data.List (find)
import System.FilePath

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Network.Wai
import Network.Wai.Parse
import Servant.API

-- During datablock upload, we would like to begin processing the file contents.
type DataBlockUploadH =
    "datablock_upload"
        :> ReqBody '[OctetStream] L.ByteString
        -- :> Files
        -- :> DrupalAuth
        :> Post '[JSON] DataBlockUploadResp

dataBlockUploadH :: TrebServer DataBlockUploadH
dataBlockUploadH body = do -- (params, files) = do
    let x = BL.lines body
    liftIO $ putStrLn "Begin"
    mapM_ (const $ liftIO $ putStrLn "progress") x
    return $ DataBlockUploadResp "test" []
    -- liftIO $ mapM_ ppFile files
    --let Just dbf = find (\(name, _) -> name == "datablock-file") files
    ---- liftIO $ mapM_ print params
    --let dbContent = fileContent $ snd dbf
    --let rows = map (BL.split ',') $ BL.lines dbContent :: [[BL.ByteString]]
    --let colTys = tallyRowHeuristic $ tail rows
    --return $ DataBlockUploadResp
    --    (T.pack $ takeBaseName $ T.unpack $ decodeUtf8 $ fileName $ snd dbf)
    --    (zip (map (decodeUtf8 . BL.toStrict) $ head rows) colTys)
    -- liftIO $ print disp

ppFile :: File FilePath -> IO ()
ppFile (name, fileinfo) = do
  putStrLn $ "Input name: " ++ show name
  putStrLn $ "File name: " ++ show (fileName fileinfo)
  putStrLn $ "Content type: " ++ show (fileContentType fileinfo)
  putStrLn $ "------- Content --------"
  readFile (fileContent fileinfo) >>= putStrLn
  putStrLn $ "------------------------"


--fileUploadH :: TrebServer FileUploadH
--fileUploadH uploadId uploadContent = drupalAuth $ do
--  activeUploads <- getActiveUploads
--  user <- getCurrentUser
--  let key = (userName user, uploadId)
--  -- TODO: Track file upload handlers and give more helpful error feedback accordingly. Should be able to tell if the upload existed and if so, whether it expired or has already been used.
--  maybe
--          (serverError "Upload path invalid. The file upload handle has expired, has already been used, or has never existed.")
--          (\(TrebServerUpload uploadHandler) -> do
--             modifyActiveUploads $ M.delete key
--             liftM toJSON $ uploadHandler uploadContent)
--          (M.lookup key activeUploads)
--
--newFileUpload :: forall a. ToJSON a => (B.ByteString -> TrebServerBase a) -> TrebServerBase URI
--newFileUpload handler = do
--    user <- getCurrentUser
--    uploadId <- freshUploadId
--    -- Add the fresh Upload ID to the active upload map.
--    reader trebEnvActiveUploads >>= liftIO . atomically . flip modifyTVar (M.insert (userName user, uploadId) $ TrebServerUpload handler)
--    baseURI <- getBaseURI
--    let p = Proxy :: Proxy FileUploadH
--    return $ safeLink p p uploadId `relativeTo` baseURI
--
data DataBlockUploadResp = DataBlockUploadResp
    { name    :: T.Text
    , columns :: [(T.Text, Maybe ProtoCellType)] }

instance ToJSON DataBlockUploadResp where
    toJSON (DataBlockUploadResp name col) =
        object [ "name"    .= name
               , "columns" .= map (\(n, ty) -> object [ "name" .= n, "type" .= ty ]) col ]
