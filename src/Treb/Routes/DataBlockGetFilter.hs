{-|
Module:      Treb.Routes.DataBlockGetFilter
Description: Trebuchet DataBlockGetFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockGetFilter ( DataBlockGetFilterH, dataBlockGetFilter ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockGetFilterH =
    "datablock" :> Capture "datablock_id" DataBlockId :> "filter"
        :> ReqBody '[JSON] DataBlockFilter
        :> Post '[JSON] DataBlock

dataBlockGetFilter :: TrebServer DataBlockGetFilterH
dataBlockGetFilter = undefined
