{-|
Module:      Treb.Routes.DataBlockCreate
Description: Trebuchet DataBlockCreate route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockCreate ( DataBlockCreateH, dataBlockCreate ) where
import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockCreateH =
    "datablock" :> "create"
        :> ReqBody '[JSON] DataBlockCreate
        :> Post '[JSON] DataBlock

dataBlockCreate :: TrebServer DataBlockCreateH
dataBlockCreate = undefined
