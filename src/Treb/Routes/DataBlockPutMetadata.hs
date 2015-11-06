{-|
Module:      Treb.Routes.DataBlockPutMetadata
Description: Trebuchet DataBlockPutMetadata route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockPutMetadata ( DataBlockPutMetadataH, dataBlockPutMetadataH ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockPutMetadataH =
    "datablock" :> Capture "datablock_id" DataBlockId :> "metadata"
        :> ReqBody '[JSON] DataBlockFilter
        :> Put '[JSON] DataBlock

dataBlockPutMetadataH :: TrebServer DataBlockPutMetadataH
dataBlockPutMetadataH = undefined
