{-|
Module:      Treb.Routes.Types
Description: Types used by route handlers and their callees.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, RankNTypes, ImpredicativeTypes, LiberalTypeSynonyms, ExistentialQuantification #-}

module Treb.Routes.Types
    ( module Servant
    , module Servant.Server
    , TrebServer
    , TrebServerBase
    , TrebEnv(..)
    , TrebConfig(..)
    , DrupalAuth
    , ActiveUploads
    , TrebServerUpload(..)
    , setTrebEnvJobTemplates
    , setTrebEnvDrupalMySQLConn
    , setTrebEnvPgPool
    , setTrebEnvUsername
    , setTrebEnvConfig
    , setTrebEnvActiveUploads
    , setTrebEnvUploadIdGen
    , setTrebEnvCurrentUser ) where

import qualified Database.MySQL.Simple as MySQL
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Control.Concurrent.STM
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.Map (Map)
import Data.Text (Text)
import Hasql (Pool)
import Hasql.Postgres (Postgres)
import Servant
import Servant.Server
import System.Random
import Treb.Types

---- Core Types ----
type TrebServer layout = ServerT layout TrebServerBase
type TrebServerBase = ReaderT TrebEnv (EitherT ServantErr IO)

data TrebEnv = TrebEnv
  { trebEnvConfig :: TrebConfig
  , trebEnvJobTemplates :: TVar [JobTemplate]
    -- ^ This TVar is written to upon inotify events in the job templates
    -- directory.
  , trebEnvDrupalMySQLConn :: Maybe MySQL.Connection
    -- ^ This is intended for authentication.
  , trebEnvPgPool :: Pool Postgres
  , trebEnvUsername :: Maybe Text -- ^ Temporary. To be replaced by trebEnvUser
  , trebEnvActiveUploads :: TVar ActiveUploads
  , trebEnvUploadIdGen :: TVar StdGen
  , trebEnvCurrentUser :: Maybe User
  , trebEnvBaseURI :: URI
  }

data TrebConfig = TrebConfig
  { confDebugMode      :: Bool
  , confPort           :: Int
  , confJobTemplateDir :: String
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

-- Record Mutators --
setTrebEnvJobTemplates    x env = env { trebEnvJobTemplates    = x }
setTrebEnvDrupalMySQLConn x env = env { trebEnvDrupalMySQLConn = x }
setTrebEnvPgPool          x env = env { trebEnvPgPool          = x }
setTrebEnvUsername        x env = env { trebEnvUsername        = x }
setTrebEnvConfig          x env = env { trebEnvConfig          = x }
setTrebEnvActiveUploads   x env = env { trebEnvActiveUploads   = x }
setTrebEnvUploadIdGen     x env = env { trebEnvUploadIdGen     = x }
setTrebEnvCurrentUser     x env = env { trebEnvCurrentUser     = x }
