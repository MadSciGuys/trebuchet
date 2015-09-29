{-|
Module:      Treb.Routes.Types
Description: Types used by route handlers and their callees.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.Types where

import Servant
import Data.Text (Text)

---- Core Types ----
type TrebServer layout = ServerT layout TrebServerBase
type TrebServerBase = ReaderT TrebEnv (EitherT ServantErr IO)

---- Helper Types ----
type DrupalAuth = Header "Cookie" Text
