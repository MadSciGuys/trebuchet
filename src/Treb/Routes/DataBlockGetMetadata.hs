{-|
Module:      Treb.Routes.DataBlockGetMetadata
Description: Trebuchet DataBlockGetMetadata route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.DataBlockGetMetadata ( DataBlockGetMetadata, dataBlockGetMetadata ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type DataBlockGetMetadata =
    -- TODO

dataBlockGetMetadata :: TrebServer DataBlockGetMetadata
dataBlockGetMetadata = undefined
