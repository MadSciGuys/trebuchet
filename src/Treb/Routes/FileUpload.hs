{-|
Module:      Treb.Routes.FileUpload
Description: Trebuchet FileUpload route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings,
             ExistentialQuantification #-}

module Treb.Routes.FileUpload
    ( FileUploadH
    , fileUploadH
    , newFileUpload
    ) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Either ()
import Data.Aeson (Value, ToJSON, toJSON)
import Servant.API
import Treb.Routes.Helpers
import Treb.Routes.Types
import Treb.Types
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar ()
import Network.URI

type FileUploadH =
    "file_upload" :> Capture "upload_id" Int
        :> ReqBody '[OctetStream] B.ByteString
        :> DrupalAuth
        :> Post '[JSON] Value

fileUploadH :: TrebServer FileUploadH
fileUploadH uploadId uploadContent = drupalAuth $ do
  activeUploads <- getActiveUploads
  user <- getCurrentUser
  let key = (userName user, uploadId)
  -- TODO: Track file upload handlers and give more helpful error feedback accordingly. Should be able to tell if the upload existed and if so, whether it expired or has already been used.
  maybe
          (serverError "Upload path invalid. The file upload handle has expired, has already been used, or has never existed.")
          (\(TrebServerUpload uploadHandler) -> do
             modifyActiveUploads $ M.delete key
             liftM toJSON $ uploadHandler uploadContent)
          (M.lookup key activeUploads)

newFileUpload :: forall a. ToJSON a => (B.ByteString -> TrebServerBase a) -> TrebServerBase URI
newFileUpload handler = do
    user <- getCurrentUser
    uploadId <- freshUploadId
    -- Add the fresh Upload ID to the active upload map.
    reader trebEnvActiveUploads >>= liftIO . atomically . flip modifyTVar (M.insert (userName user, uploadId) $ TrebServerUpload handler)
    baseURI <- getBaseURI
    let p = Proxy :: Proxy FileUploadH
    return $ safeLink p p uploadId `relativeTo` baseURI
