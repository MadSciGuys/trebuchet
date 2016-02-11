{-|
Module:      Treb.Routes.Types
Description: Types used by route handlers and their callees.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, RankNTypes,
             ImpredicativeTypes, LiberalTypeSynonyms,
             ExistentialQuantification #-}

module Treb.Routes.Types
    ( TrebServer
    , TrebServerBase
    , TrebConfig(..)
    , DrupalAuth
    , ActiveUploads
    , TrebServerUpload(..)
    ) where

import qualified Database.MySQL.Simple as MySQL
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Control.Concurrent.STM
import Control.Monad.Reader (ReaderT)
import Data.Map (Map)
import Data.Text (Text)
import Hasql (Pool)
import Hasql.Postgres (Postgres)
import Servant
import Servant.Server
import System.Random
import Treb.Types
import Control.Monad.Trans.Except

---- Core Types ----
type TrebServer layout = ServerT layout TrebServerBase
type TrebServerBase = ReaderT TrebEnv (ExceptT ServantErr IO)

data TrebConfig = TrebConfig
  { confDebugMode      :: Maybe Bool
  , confPort           :: Maybe String
  , confJobTemplateDir :: Maybe String
  , confSSLCertPath    :: Maybe String
  , confSSLCertKeyPath :: Maybe String
  , confOAHost         :: Maybe String
  , confOAPort         :: Maybe String
  , confOADatabase     :: Maybe String
  , confOAUsername     :: Maybe String
  , confOAPassword     :: Maybe String
  , confOADomain       :: Maybe String
  , confPGHost         :: Maybe String
  , confPGPort         :: Maybe String
  , confPGUsername     :: Maybe String
  , confPGPassword     :: Maybe String
  , confPGDatabase     :: Maybe String
  , confPGPoolMax      :: Maybe String
  , confPGConnLifetime :: Maybe String
  , confBaseURI        :: Maybe String }

---- Helper Types ----
type DrupalAuth = Header "Cookie" Text

type ActiveUploads = Map (Text, Int) TrebServerUpload

data TrebServerUpload = forall a. A.ToJSON a => TrebServerUpload (B.ByteString -> TrebServerBase a)
