{-|
Module:      Treb.Routes.DataBlockPutMetadata
Description: Trebuchet DataBlockPutMetadata route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.DataBlockPutMetadata ( DataBlockPutMetadata, dataBlockPutMetadata ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockPutMetadata =
    "datablock" :> Capture "datablock_id" DataBlockId :> "metadata"
        :> ReqBody '[JSON] DataBlockFilter
        :> Put '[JSON] DataBlock

dataBlockPutMetadata :: TrebServer DataBlockPutMetadata
dataBlockPutMetadata = undefined
