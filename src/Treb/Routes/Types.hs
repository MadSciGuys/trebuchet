{-|
Module:      Treb.Routes.Types
Description: Types used by route handlers and their callees.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.Types
    ( module Servant
    , module Servant.Server
    , TrebServer
    , TrebServerBase
    , DrupalAuth ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.Text (Text)
import Treb.Types (TrebEnv)

import Servant
import Servant.Server

---- Core Types ----
type TrebServer layout = ServerT layout TrebServerBase
type TrebServerBase = ReaderT TrebEnv (EitherT ServantErr IO)

---- Helper Types ----
type DrupalAuth = Header "Cookie" Text
