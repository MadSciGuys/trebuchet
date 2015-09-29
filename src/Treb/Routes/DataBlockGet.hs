{-|
Module:      Treb.Routes.DataBlockGet
Description: Trebuchet DataBlockGet route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockGet ( DataBlockGetH, dataBlockGet ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockGetH =
    "datablock" :> Capture "datablock_id" DataBlockId
        :> Get '[JSON] DataBlock

dataBlockGet :: TrebServer DataBlockGetH
dataBlockGet = undefined
