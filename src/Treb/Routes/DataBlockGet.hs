{-|
Module:      Treb.Routes.DataBlockGet
Description: Trebuchet DataBlockGet route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockGet
    ( DataBlockGetH
    , dataBlockGetH
    ) where

import Servant.API
import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockGetH =
    "datablock" :> Capture "datablock_id" DataBlockId
        :> Get '[JSON] DataBlock

dataBlockGetH :: TrebServer DataBlockGetH
dataBlockGetH = undefined
