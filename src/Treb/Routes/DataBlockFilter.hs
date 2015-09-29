{-|
Module:      Treb.Routes.DataBlockFilter
Description: Trebuchet DataBlockFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.DataBlockFilter ( DataBlockFilter, dataBlockFilter ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockFilter =
    "datablock" :> "filter"
        :> ReqBody '[JSON] DataBlockSetFilter
        :> Post '[JSON] DataBlock

dataBlockFilter :: TrebServer DataBlockFilter
dataBlockFilter = undefined
