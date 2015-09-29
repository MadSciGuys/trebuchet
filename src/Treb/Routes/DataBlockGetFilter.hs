{-|
Module:      Treb.Routes.DataBlockGetFilter
Description: Trebuchet DataBlockGetFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.DataBlockGetFilter ( DataBlockGetFilter, dataBlockGetFilter ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockGetFilter =
    "datablock" :> Capture "datablock_id" DataBlockId :> "filter"
        :> ReqBody '[JSON] DataBlockFilter
        :> Post '[JSON] DataBlock

dataBlockGetFilter :: TrebServer DataBlockGetFilter
dataBlockGetFilter = undefined
