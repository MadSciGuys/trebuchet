{-|
Module:      Treb.Routes.DataBlockFilter
Description: Trebuchet DataBlockFilter route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockFilter
    ( DataBlockFilterH
    , dataBlockFilterH
    ) where

import Servant.API
import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockFilterH =
    "datablock" :> "filter"
        :> ReqBody '[JSON] DataBlockFilter
        :> Post '[JSON] DataBlock

dataBlockFilterH :: TrebServer DataBlockFilterH
dataBlockFilterH = undefined
