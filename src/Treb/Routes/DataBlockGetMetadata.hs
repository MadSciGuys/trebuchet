{-|
Module:      Treb.Routes.DataBlockGetMetadata
Description: Trebuchet DataBlockGetMetadata route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockGetMetadata ( DataBlockGetMetadataH, dataBlockGetMetadata ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockGetMetadataH =
    "datablock" :> Capture "datablock_id" DataBlockId :> "metadata"
        :> Get '[JSON] DataBlock

dataBlockGetMetadata :: TrebServer DataBlockGetMetadataH
dataBlockGetMetadata = undefined
