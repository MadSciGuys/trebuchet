{-|
Module:      Treb.Env
Description: Environment initializer and types for the Trebuchet server.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE QuasiQuotes, ExistentialQuantification, Rank2Types,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}

module Treb.Env where

import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.QueryParams as MySQL
import qualified Database.MySQL.Simple.QueryResults as MySQL
import qualified Hasql as H
import qualified Hasql.Backend as HB
import qualified Hasql.Postgres as HP
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Exception
import Data.Bifunctor
import Data.Bool
import Data.Word
import Data.Functor.Identity
import System.FilePath
import Treb.Config
import Treb.DB.Statements
import Treb.Filter
import Treb.Types
import Treb.Combinators

---- Types ----

-- | Initial Trebuchet state. The purpose of this type is to separate state
--   intended for HTTP handlers from state which is ideally inaccessible to
--   those handlers. Ideally, MVars for example are kept out of reach of HTTP
--   handlers as some manipulations of MVars can be challenging to debug and
--   manage. This attempts to keep all such operations on such MVars or similar
--   explicit and in one place.
data TrebInit = TrebInit
    { trebInitPgClose    :: IO ()
      -- ^ Close PostgreSQL connections.
    , trebInitMySQLClose :: IO ()
      -- ^ Close OpenAtrium MySQL connections.
    , trebInitEnv        :: TrebEnv
      -- ^ Envorionment for Trebuchet HTTP handlers.
    }

-- | Environment available to all Trebuchet HTTP handlers via a ReaderT
--   or similar in the TrebServer type.
data TrebEnv = TrebEnv
    { trebEnvMode            :: TrebMode
      -- ^ Determines whether Trebuchet is in a debugging or production mode.
    , trebEnvFindDataBlock   :: DataBlockName
                             -> IO (Maybe DataBlock)
      -- ^ Find a datablock by its name.
    , trebEnvFilterDataBlock :: Filter (M.Map DataBlockName DataBlock)
                             -> IO (M.Map DataBlockName DataBlock)
      -- ^ Filter datablocks by an arbitrarily complex boolean expression with
      --   DataBlockFilter atom predicates.
    , trebEnvNewDataBlock    :: NewDataBlock -> BL.ByteString -> IO ()
      -- ^ Store a new datablock given its metadata and complete protocol buffer
      --   payload.
    , trebEnvPgSession       :: forall m a. H.Session HP.Postgres m a -> m (Either (H.SessionError HP.Postgres) a)
      -- ^ Execute a PostgreSQL "session" as defined by Hasql.
    , trebEnvOAMySQLConn       :: Either String MySQL.Connection
      -- ^ MySQL connection to OpenAtrium. Left contains a description of the
      --   reason for having no MySQL connection.
    }

-- | Trebuchet mode. Trebuchet can be in either a debugging or production mode.
--
--   While this could have been a boolean type, I would prefer to later support
--   other modes without having to change code that may use the trebEnvMode
--   field of our TrebEnv type.
data TrebMode =
      Debug
      -- ^ Debugging mode. Enables verbose log messages and disables all
      --   OpenAtrium-dependent actions.
    | Production
      -- ^ Production mode. Disables verbose log messages and enables all
      --   OpenAtrium-dependent actions.

---- Functions ----

-- | Initialize Trebuchet and assemble the environment data needed by its HTTP
--   handlers.
getTrebInit :: RunConfig -> IO (Either String TrebInit)
getTrebInit config = runExceptT $ do
    -- Determine mode according to the command line debug-mode flag.
    let trebMode = bool Production Debug (confDebugMode config)
    -- Construct reader data for initializers
    pg    <- initPostgreSQL config
    mysql <-
        if confDebugMode config then
            return $ Left "Connections to OpenAtrium MySQL is disallowed in Trebuchet debug mode."
        else
            Right <$> initOAMySQL config
    dbMap <- initDataBlockMap config pg

    return $
        TrebInit
            (H.releasePool pg)
            (either
                (putStrLn . ("No OpenAtrium MySQL connection. Cannot close connection if one does not exist.\n  Reason: " ++))
                MySQL.close
                mysql) $
            TrebEnv
                trebMode
                (liftM2 (flip M.lookup) (readMVar dbMap) . return)
                (liftM2 (flip appFilter) (readMVar dbMap) . return)
                (newDataBlock trebMode pg mysql dbMap config)
                (H.session pg)
                mysql

-- | Initialize a connection to the OpenAtrium MySQL database.
initOAMySQL :: RunConfig
            -> ExceptT String IO MySQL.Connection
initOAMySQL config = do
    case confOA config of
        Nothing ->
            throwE "OpenAtrium configuration is not set while in production mode."
        Just conf ->
            ExceptT $ do
                putStrLn "Connecting to OpenAtrium MySQL database."
                fmap (first (show :: SomeException -> String)) $ try $ MySQL.connect $ MySQL.defaultConnectInfo
                    { MySQL.connectHost     = confOAHost conf
                    , MySQL.connectPort     = confOAPort conf
                    , MySQL.connectDatabase = confOADatabase conf
                    , MySQL.connectUser     = confOAUsername conf
                    , MySQL.connectPassword = confOAPassword conf }

-- | Initialize a pool of PostgreSQL connections and check that the connection
--   parameters are valid before proceeding.
initPostgreSQL :: RunConfig
               -> ExceptT String IO (H.Pool HP.Postgres)
initPostgreSQL config =
    case poolSettings of
        Nothing ->
            throwE "Invalid --pg-pool-max or --pg-conn-lifetime."
        Just ps -> do
            -- Acquire a pool of PostgreSQL connections
            pool <- liftIO $ H.acquirePool cxSettings ps
            -- Run trivial query to force connection parameter validation
            response <- liftIO $ querySingle pool [H.stmt|select 1|]
            case response of
                Left err ->
                    throwE $ showSessionError err
                Right (1 :: Word64) -> do
                    liftIO $ putStrLn "PostgreSQL successfully initialized."
                    return pool
                Right _ ->
                    throwE "Invalid response received from PostgreSQL ping query."
    where
        querySingle :: HB.CxValue HP.Postgres r =>
                       H.Pool HP.Postgres
                    -> H.Stmt HP.Postgres
                    -> IO (Either (H.SessionError HP.Postgres) r)
        querySingle pool stmt =
            fmap (fmap runIdentity) $ liftIO $
                H.session pool $
                    H.tx (Just (H.Serializable, Nothing)) $
                        H.singleEx stmt
        cxSettings =
            HP.ParamSettings
                (BC.pack $ confPGHost $ confPG config)
                (confPGPort $ confPG config)
                (BC.pack $ confPGUsername $ confPG config)
                (BC.pack $ confPGPassword $ confPG config)
                (BC.pack $ confPGDatabase $ confPG config)
        poolSettings =
            H.poolSettings
                (confPGPoolMax $ confPG config)
                (confPGConnLifetime $ confPG config)

-- | Generate an initial datablock map, reflecting the datablocks known in
--   PostgreSQL. This will search for all protocol buffer datablock payloads on
--   disk.
initDataBlockMap :: RunConfig
                 -> H.Pool HP.Postgres
                 -> ExceptT String IO DataBlockMap
initDataBlockMap config pg = return undefined

-- | Create a new datablock, thereby creating a persistent datablock on disk,
--   updating PostgreSQL, and updating the in-memory datablock map.
newDataBlock :: TrebMode
             -> H.Pool HP.Postgres
             -> Either String MySQL.Connection
             -> DataBlockMap
             -> RunConfig
             -> NewDataBlock
             -> BL.ByteString
             -> IO ()
newDataBlock Debug pg _ dbMap config ndb bs = do
    -- Add datablock to PostgreSQL.
    dbId :: Word64
      <- fmap (either (error . show) runIdentity) $ H.session pg $
        H.tx (Just (H.Serializable, Just True)) $
            H.singleEx $ mkDataBlock $ ndbName ndb
    -- Write datablock to file.
    BL.writeFile (confDataBlockDir config </> show dbId ++ ".db") bs
    -- Get a new reference count TVar.
    dbRefs <- newTVarIO 0
    -- Add datablock to the global datablock map.
    modifyMVar_ dbMap $ (return .) $ M.insert (ndbName ndb) $
        DataBlock
            (ndbName ndb)
            dbId
            Nothing
            dbRefs
            (ndbFields ndb)
            undefined -- TODO: dbIndex
            undefined -- TODO: dbMmap
            undefined -- TODO: dbMsize
            undefined -- TODO: dbRecs
newDataBlock Production pg (Left s) dbMap config ndb bs = do
    putStrLn $ "No OpenAtrium MySQL connection. Cannot create a DataBlock in Trebuchet production mode with an OpenAtrium MySQL connection.\n  Reason: " ++ s
newDataBlock Production pg (Right mysql) dbMap config ndb bs = do
    -- Add datablock to PostgreSQL.
    dbId :: Word64
      <- fmap (either (error . show) runIdentity) $ H.session pg $
        H.tx (Just (H.Serializable, Just True)) $
            H.singleEx $ mkDataBlock $ ndbName ndb
    -- Write datablock to file.
    BL.writeFile (confDataBlockDir config </> show dbId ++ ".db") bs
    -- Get a new reference count TVar.
    dbRefs <- newTVarIO 0
    -- Get additional information about the datablock owner.
    users <- MySQL.query mysql
        "SELECT atrium_users.uid, atrium_users.name, atrium_realname.realname, atrium_users.mail FROM atrium_users INNER JOIN atrium_realname ON atrium_realname.uid = atrium_users.uid WHERE atrium_users.uid = ?"
        (MySQL.Only $ ndbOwner ndb)
    -- Add datablock to the global datablock map.
    modifyMVar_ dbMap $ (return .) $ M.insert (ndbName ndb) $
        DataBlock
            (ndbName ndb)
            dbId
            (case users of
                [(uid, username, name, email)] -> Just $ User uid username name email
                [] -> error $ "User with uid " ++ show (ndbOwner ndb) ++ " not found in OpenAtrium MySQL database.")
            dbRefs
            (ndbFields ndb)
            undefined -- TODO: dbIndex
            undefined -- TODO: dbMmap
            undefined -- TODO: dbMsize
            undefined -- TODO: dbRecs
