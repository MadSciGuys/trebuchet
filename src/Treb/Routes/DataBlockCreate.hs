{-|
Module:      Treb.Routes.DataBlockCreate
Description: Trebuchet DataBlockCreate route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.DataBlockCreate ( DataBlockCreate, dataBlockCreate ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockCreate =
    "datablock" :> "create"
        :> ReqBody '[JSON] DataBlockCreate
        :> Post '[JSON] DataBlock

dataBlockCreate :: TrebServer DataBlockCreate
dataBlockCreate = undefined
