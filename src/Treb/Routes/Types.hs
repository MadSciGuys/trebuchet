{-|
Module:      Treb.Routes.Types
Description: Types used by route handlers and their callees.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, RankNTypes, ImpredicativeTypes, LiberalTypeSynonyms #-}

module Treb.Routes.Types
    ( module Servant
    , module Servant.Server
    , TrebServer
    , TrebServerBase
    , TrebEnv(..)
    , DrupalAuth
    , FileUploadH
    , setTrebEnvJobTemplates
    , setTrebEnvDrupalMySQLConn
    , setTrebEnvPgPool
    , setTrebEnvUsername
    , setTrebEnvConfig
    , setTrebEnvActiveUploads
    , setTrebEnvUploadIdGen
    , setTrebEnvCurrentUser ) where

import Data.ByteString (ByteString)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.Text (Text)
import Treb.Types
import Control.Concurrent.STM
import System.Random

import Data.Map (Map)
import Hasql (Pool)
import Hasql.Postgres (Postgres)

import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.Types as MySQL

import Servant
import Servant.Server

---- Core Types ----
type TrebServer layout = ServerT layout TrebServerBase
type TrebServerBase = ReaderT TrebEnv (EitherT ServantErr IO)

data TrebEnv = TrebEnv
  { trebEnvJobTemplates :: TVar [JobTemplate]
    -- ^ This TVar is written to upon inotify events in the job templates
    -- directory.
  , trebEnvDrupalMySQLConn :: Maybe MySQL.Connection
    -- ^ This is intended for authentication.
  , trebEnvPgPool :: Pool Postgres
  , trebEnvUsername :: Maybe Text -- ^ Temporary. To be replaced by trebEnvUser
  , trebEnvConfig :: TrebConfig
  , trebEnvActiveUploads :: TVar (Map Int (TrebServer (forall a. FileUploadH a)))
  , trebEnvUploadIdGen :: TVar StdGen
  , trebEnvCurrentUser :: Maybe User
  }

---- Helper Types ----
type DrupalAuth = Header "Cookie" Text

-- | File upload handler Servant layout type.
type FileUploadH ret =
    "file_upload" :> Capture "upload_id" Int
        :> ReqBody '[OctetStream] ByteString
        :> DrupalAuth
        :> Post '[JSON] ret

-- Record Mutators --
setTrebEnvJobTemplates    x env = env { trebEnvJobTemplates    = x }
setTrebEnvDrupalMySQLConn x env = env { trebEnvDrupalMySQLConn = x }
setTrebEnvPgPool          x env = env { trebEnvPgPool          = x }
setTrebEnvUsername        x env = env { trebEnvUsername        = x }
setTrebEnvConfig          x env = env { trebEnvConfig          = x }
setTrebEnvActiveUploads   x env = env { trebEnvActiveUploads   = x }
setTrebEnvUploadIdGen     x env = env { trebEnvUploadIdGen     = x }
setTrebEnvCurrentUser     x env = env { trebEnvCurrentUser     = x }
