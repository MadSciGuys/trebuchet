{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes, FlexibleContexts #-}
module Treb.DB.Schema where

import qualified Hasql as H
import qualified Hasql.Postgres as HP
import Control.Monad.Trans.Control

type SchemaInit = forall s. H.Tx HP.Postgres s ()

-- Configure --
connSettings :: HP.Settings
connSettings = HP.ParamSettings "10.37.49.94" 5432 "trebuchet" "trebuchet" "trebuchet"

poolSettings :: Maybe H.PoolSettings
poolSettings = H.poolSettings 6 30

getPool :: IO (H.Pool HP.Postgres)
getPool = maybe (fail "Pool settings invalid!") (H.acquirePool connSettings) poolSettings
    
main :: IO ()
main = do
  pool <- getPool
  x <- H.session pool schemaInit
  print x
  return ()

schemaInit :: (MonadBaseControl IO m) => H.Session HP.Postgres m ()
schemaInit = do
  let serializableMode = Just (H.Serializable, Just True)
  let sqlExec = H.tx serializableMode

  sqlExec job_status_t
  sqlExec job
  sqlExec job_argument_type_t
  sqlExec job_argument
  sqlExec input_datablock
  sqlExec datablock_name_type_t
  sqlExec datablock_name_t
  sqlExec datablock_source_type_t
  sqlExec datablock_source_t
  sqlExec datablock
  sqlExec data_pipeline

job_status_t :: SchemaInit
job_status_t = mapM_ H.unitEx
  [ [H.stmt| drop type if exists "job_status_t" cascade |]
  , [H.stmt| create type "job_status_t" as enum
      ( 'running'
      , 'success'
      , 'failed'
      , 'canceled' ) |] ]

job :: SchemaInit
job = mapM_ H.unitEx
  [ [H.stmt| drop table if exists "job" cascade |]
  , [H.stmt| create table "job"
      ( "id"                  bigserial primary key
      , "owner_id"            bigint not null
      , "template_id"         bigint not null
      , "name"                varchar
      , "status"              job_status_t not null default 'running'
      , "start_time"          timestamptz not null
      , "end_time"            timestamptz
      , "output_datablock_id" bigint
      , "failure_reason"      varchar ) |] ]

job_argument_type_t :: SchemaInit
job_argument_type_t = mapM_ H.unitEx
  [ [H.stmt| drop type if exists "job_argument_type_t" cascade |]
  , [H.stmt| create type "job_argument_type_t" as enum
      ( 'bool'
      , 'string'
      , 'int'
      , 'real'
      , 'datetime'
      , 'vector' ) |] ]

job_argument :: SchemaInit
job_argument = mapM_ H.unitEx
  [ [H.stmt| drop table if exists "job_argument" cascade |]
  , [H.stmt| create table "job_argument"
      ( "id"             bigserial primary key
      , "job_id"         bigint not null
      , "name"           varchar
      , "type"           job_argument_type_t
      , "value_bool"     boolean
      , "value_string"   varchar
      , "value_int"      bigint
      , "value_real"     double precision
      , "value_datetime" timestamptz
      , "value_vector"   bigint[] ) |] ]

input_datablock :: SchemaInit
input_datablock = mapM_ H.unitEx
  [ [H.stmt| drop table if exists "input_datablock" cascade |]
  , [H.stmt| create table "input_datablock"
      ( "job_id"           bigint not null
      , "datablock_id"     bigint not null
      , "datablock_key"    varchar
      , "datablock_filter" text ) |] ]

datablock_name_type_t :: SchemaInit
datablock_name_type_t = mapM_ H.unitEx
  [ [H.stmt| drop type if exists "datablock_name_type_t" cascade |]
  , [H.stmt| create type "datablock_name_type_t" as enum
      ( 'ad_hoc'
      , 'recipe'
      , 'job_result'
      , 'alias' ) |] ]

datablock_name_t :: SchemaInit
datablock_name_t = mapM_ H.unitEx
  [ [H.stmt| drop type if exists "datablock_name_t" cascade |]
  , [H.stmt| create type "datablock_name_t" as
      ( "type"          datablock_name_type_t
      , "given_name"    varchar
      , "compound_name" varchar
      , "recipe_names"  varchar[]
      , "job_id"        bigint ) |] ]

datablock_source_type_t :: SchemaInit
datablock_source_type_t = mapM_ H.unitEx
  [ [H.stmt| drop type if exists "datablock_source_type_t" cascade |]
  , [H.stmt| create type "datablock_source_type_t" as enum
      ( 'user'
      , 'job'
      , 'data_pipeline' ) |] ]

datablock_source_t :: SchemaInit
datablock_source_t = mapM_ H.unitEx
  [ [H.stmt| drop type if exists "datablock_source_t" cascade |]
  , [H.stmt| create type "datablock_source_t" as
      ( "type"            datablock_source_type_t
      , "source_username" varchar
      , "source_id"       bigint ) |] ]

datablock :: SchemaInit
datablock = mapM_ H.unitEx
  [ [H.stmt| drop table if exists "datablock" cascade |]
  , [H.stmt| create table "datablock"
      ( "id"               bigserial primary key
      , "datablock_name"   datablock_name_t not null
      , "datablock_source" datablock_source_t ) |] ]

datablock_name :: SchemaInit
datablock_name = mapM_ H.unitEx
  [ [H.stmt| drop table if exists "datablock_name" cascade |]
  , [H.stmt| create table "datablock_name"
      ( "id" bigserial primary key
      , "type" datablock_name_type_t) |] ]

data_pipeline :: SchemaInit
data_pipeline = mapM_ H.unitEx
  [ [H.stmt| drop table if exists "data_pipeline" cascade |]
  , [H.stmt| create table "data_pipeline"
      ( "id"   bigserial primary key
      , "name" varchar not null ) |] ]

-- Trebuchet internal users are presently disabled. --
--
-- TODO: Add supporting code for this alternative to Drupal users and authentication.
--
-- user :: SchemaInit
-- user = mapM_ H.unitEx
--   [ [H.stmt| drop table if exists "user" cascade |]
--   , [H.stmt| create table "user"
--       ( "id"       bigserial primary key
--       , "username" varchar not null
--       , "name"     varchar not null
--       , "email"    varchar not null ) |] ]
