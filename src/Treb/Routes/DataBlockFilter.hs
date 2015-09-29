{-|
Module:      Treb.Routes.DataBlockFilter
Description: Trebuchet DataBlockFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockFilter ( DataBlockFilterH, dataBlockFilter ) where
import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockFilterH =
    "datablock" :> "filter"
        :> ReqBody '[JSON] DataBlockSetFilter
        :> Post '[JSON] DataBlock

dataBlockFilter :: TrebServer DataBlockFilterH
dataBlockFilter = undefined
