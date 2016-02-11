{-|
Module:      Treb.Routes.DataBlockCreate
Description: Trebuchet DataBlockCreate route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, QuasiQuotes #-}

module Treb.Routes.DataBlockCreate
    ( DataBlockCreateH
    , dataBlockCreateH
    ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString as A
import qualified Hasql as H
import Control.Monad.IO.Class
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Either ()
import Control.Monad.Identity
import Data.CSV.Conduit
import Data.ProtoBlob
import ProtoDB.Parser
import ProtoDB.Types
import ProtoDB.Writer
import Servant.API
import Treb.Routes.Helpers
import Treb.Routes.Types
import Treb.Types
import Treb.Routes.FileUpload

---- Route-Specific Type ----
type DataBlockCreateH =
    "datablock" :> "create"
        :> ReqBody '[JSON] DataBlockCreateMsg
        :> DrupalAuth
        :> Post '[JSON] (NoWrapEither DataBlockFileUploadMsg DataBlockMetadataMsg)

dataBlockCreateH :: TrebServer DataBlockCreateH
dataBlockCreateH msg@(DataBlockCreateMsg name maybeFields maybeRecords) = drupalAuth undefined
