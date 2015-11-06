{-|
Module:      Treb.DB.Schema
Description: PostgreSQL database schema (de-)initializers for Trebuchet.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE QuasiQuotes, OverloadedStrings, Rank2Types, ScopedTypeVariables #-}
module Treb.DB.Schema where

import qualified Data.ByteString.Char8 as BC (pack)
import qualified Hasql as H
import qualified Hasql.Postgres as HP
import Control.Exception (bracket)
import Control.Monad.IO.Class
import System.Exit (die)
import Treb.Config
import Treb.Combinators

---- Types ----

-- | A PostgreSQL Hasql statement.
type Stmt = H.Stmt HP.Postgres

-- | Contains a PostgreSQL database migration, including a name and both "up"
--   and "down" statements.
data Migration
    = Schema String Stmt Stmt
    | Table  String Stmt Stmt
    | Type   String Stmt Stmt
    | Query  String Stmt

---- Exported Functions ----

-- | Initialize the Trebuchet PostgreSQL schema. This will not create the
--   database. That is assumed to be already created.
initSchema :: PGConfig -> IO ()
initSchema config = do
    putStrLn "> Initialize Trebuchet Schema"
    pgSession config $ do
        up schema
        up set_search_path
        up job_status_t
        up job
        up job_argument_type_t
        up job_argument
        up input_datablock
        up datablock_name_type_t
        up datablock_name_t
        up datablock

-- | Drop the Trebuchet PostgreSQL schema.
dropSchema :: PGConfig -> IO ()
dropSchema config = do
    putStrLn "> Drop Trebuchet Schema"
    pgSession config $ down schema

---- Migrations ----

schema :: Migration
schema =  Schema "trebuchet"
    [H.stmt| drop schema "trebuchet" cascade |]
    [H.stmt| create schema "trebuchet" |]

job_status_t :: Migration
job_status_t =  Type "job_status_t"
    [H.stmt| drop type if exists "job_status_t" cascade |]
    [H.stmt| create type "job_status_t" as enum
        ( 'running'
        , 'success'
        , 'failed'
        , 'canceled' ) |]

job :: Migration
job =  Table "job"
    [H.stmt| drop table if exists "job" cascade |]
    [H.stmt| create table "job"
      ( "id"                  bigserial primary key
      , "owner_id"            bigint not null
      , "template_id"         bigint not null
      , "name"                varchar
      , "status"              job_status_t not null default 'running'
      , "start_time"          timestamptz not null
      , "end_time"            timestamptz
      , "output_datablock_id" bigint
      , "failure_reason"      varchar ) |]

job_argument_type_t :: Migration
job_argument_type_t = Type "job_argument_type_t"
    [H.stmt| drop type if exists "job_argument_type_t" cascade |]
    [H.stmt| create type "job_argument_type_t" as enum
        ( 'bool'
        , 'string'
        , 'int'
        , 'real'
        , 'datetime'
        , 'vector' ) |]

job_argument :: Migration
job_argument =  Table "job_argument"
    [H.stmt| drop table if exists "job_argument" cascade |]
    [H.stmt| create table "job_argument"
        ( "id"             bigserial primary key
        , "job_id"         bigint not null
        , "name"           varchar
        , "type"           job_argument_type_t
        , "value_bool"     boolean
        , "value_string"   varchar
        , "value_int"      bigint
        , "value_real"     double precision
        , "value_datetime" timestamptz
        , "value_vector"   bigint[] ) |]

input_datablock :: Migration
input_datablock =  Table "input_datablock"
    [H.stmt| drop table if exists "input_datablock" cascade |]
    [H.stmt| create table "input_datablock"
        ( "job_id"           bigint not null
        , "datablock_id"     bigint not null
        , "datablock_key"    varchar
        , "datablock_filter" text ) |]

datablock_name_type_t :: Migration
datablock_name_type_t =  Type "datablock_name_type_t"
    [H.stmt| drop type if exists "datablock_name_type_t" cascade |]
    [H.stmt| create type "datablock_name_type_t" as enum
        ( 'ad_hoc'
        , 'recipe'
        , 'job_result'
        , 'alias' ) |]

datablock_name_t :: Migration
datablock_name_t =  Type "datablock_name_t"
    [H.stmt| drop type if exists "datablock_name_t" cascade |]
    [H.stmt| create type "datablock_name_t" as
      ( "type"          datablock_name_type_t
      , "given_name"    varchar
      , "uploader_name" varchar
      , "compound_name" varchar
      , "recipe_names"  varchar[]
      , "pipeline_name" varchar
      , "job_id"        bigint ) |]

datablock :: Migration
datablock =  Table "datablock"
    [H.stmt| drop table if exists "datablock" cascade |]
    [H.stmt| create table "datablock"
        ( "id"             bigserial primary key
        , "datablock_name" datablock_name_t not null ) |]

-- | A migration intending to change all subsequent queries to apply their
--   actions to the "trebuchet" schema. This is the default for PostgreSQL
--   users with the name "trebuchet", however, we cannot assume this so this
--   query is used.
set_search_path :: Migration
set_search_path = Query "set_search_path"
    [H.stmt| set search_path to "trebuchet" |]

---- Helpers ----

-- | Execute the "up" statement of a migration in a Hasql session.
up :: Migration -> H.Session HP.Postgres IO ()
up m = do
    liftIO $ putStrLn $ ">> UP " ++ migrationType m ++ ":" ++ migrationName m
    unitSession $ migrateUp m

-- | Execute the "down" statement of a migration in a Hasql session.
down :: Migration -> H.Session HP.Postgres IO ()
down m = do
    liftIO $ putStrLn $ ">> DOWN " ++ migrationType m ++ ":" ++ migrationName m
    unitSession $ migrateDown m

-- | Execute a statement without returning any response.
unitSession :: H.Stmt HP.Postgres -> H.Session HP.Postgres IO ()
unitSession stmt = H.tx (Just (H.Serializable, Just True)) (H.unitEx stmt :: forall s. H.Tx HP.Postgres s ())

-- | Run a PostgreSQL session with an ephemeral connection with the given
--   PostgreSQL configuration.
pgSession :: PGConfig
          -> H.Session HP.Postgres IO ()
          -> IO ()
pgSession config session =
    bracket getPool H.releasePool $ \pool -> do
        H.session pool session >>= either
            (putStrLn . showSessionError)
            (const $ return ())
    where
        getPool = case poolSettings of
            Just ps ->
                H.acquirePool cxSettings ps
            Nothing ->
                die "Invalid PostgreSQL pool settings!"

        cxSettings =
            HP.ParamSettings (BC.pack $ confPGHost config)
                             (confPGPort config)
                             (BC.pack $ confPGUsername config)
                             (BC.pack $ confPGPassword config)
                             (BC.pack $ confPGDatabase config)

        poolSettings =
            H.poolSettings
                (confPGPoolMax config)
                (confPGConnLifetime config)

-- | Unique name of a migration.
migrationName :: Migration -> String
migrationName (Schema name _ _) = name
migrationName (Table  name _ _) = name
migrationName (Type   name _ _) = name
migrationName (Query name _)    = name

-- | String representation of a migration's constructor.
migrationType :: Migration -> String
migrationType (Schema _ _ _) = "schema"
migrationType (Table  _ _ _) = "table"
migrationType (Type   _ _ _) = "type"
migrationType (Query  _ _)   = "query"

-- | Get the "up" statement of a migration.
migrateUp :: Migration -> Stmt
migrateUp (Schema _ _ up)  = up
migrateUp (Table  _ _ up)  = up
migrateUp (Type   _ _ up)  = up
migrateUp (Query  _ query) = query

-- | Get the "down" statement of a migration.
migrateDown :: Migration -> Stmt
migrateDown (Schema _ down _) = down
migrateDown (Table  _ down _) = down
migrateDown (Type   _ down _) = down
migrateDown (Query  _ query)  = query
