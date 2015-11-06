{-|
Module:      Treb.Routes.DataBlockGetMetadata
Description: Trebuchet DataBlockGetMetadata route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockGetMetadata ( DataBlockGetMetadataH, dataBlockGetMetadataH ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockGetMetadataH =
    "datablock" :> Capture "datablock_id" DataBlockId :> "metadata"
        :> Get '[JSON] DataBlock

dataBlockGetMetadataH :: TrebServer DataBlockGetMetadataH
dataBlockGetMetadataH = undefined
