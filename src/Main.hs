{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.QueryParams as MySQL
import qualified Database.MySQL.Simple.QueryResults as MySQL
import qualified Hasql as H
import qualified Hasql.Postgres as HP
import Data.Word
import Data.Aeson
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Data.Maybe
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Data.Text (Text)
import Servant
import Data.List (find)
import Web.Cookie
import Data.Text.Encoding
import Data.Bool
import Treb.Config
import Treb.Types
import Treb.Routes
import Treb.Routes.Helpers
import Treb.Routes.Types
import Data.Time.Clock
import Data.Functor.Identity

---- Important Functions ----
main :: IO ()
main =
  withTrebEnv $ \ env ->
    bool
      runSettings
      (runTLS $ fromJust $ tlsSettings <$> confSSLCertPath (trebEnvConfig env) <*> confSSLCertKeyPath (trebEnvConfig env))
      (useSSL env)
      (trebWarpSettings env)
      (trebApp env)
      
useSSL :: TrebEnv -> Bool
useSSL env = isJust (confSSLCertPath $ trebEnvConfig env) && isJust (confSSLCertKeyPath $ trebEnvConfig env)

trebWarpSettings :: TrebEnv -> Settings
trebWarpSettings env =
  foldl (flip ($)) defaultSettings
    [ setBeforeMainLoop $ putStrLn "Trebuchet is ready."
    , setPort $ confPort $ trebEnvConfig env ]

trebApp :: TrebEnv -> Application
trebApp = serve trebApiProxy . flip enter trebServer . runReaderTNat

trebServer :: TrebServer TrebApi
trebServer = dataBlockCreateH
