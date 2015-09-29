{-|
Module:      Treb.Routes.DataBlockGet
Description: Trebuchet DataBlockGet route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.DataBlockGet ( DataBlockGet, dataBlockGet ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockGet =
    "datablock" :> Capture "datablock_id" DataBlockId
        :> Get '[JSON] DataBlock

dataBlockGet :: TrebServer DataBlockGet
dataBlockGet = undefined
