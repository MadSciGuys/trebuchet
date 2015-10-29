{-|
Module:      Treb.JSON
Description: ToJSON/FromJSON Instances.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX

This module provides instances for the 'ToJSON' and 'FromJSON' Aeson
typeclasses.

Each record type JSON representation includes an initial field called "type,"
providing something like a "fully qualified name" unambigiously identifying the
record type. This is especially helpful (perhaps even necessary) when most or
all of a record's fields are optional.
-}

{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TypeFamilies, FlexibleInstances #-}

module Treb.JSON () where

import Prelude hiding (String)

import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Base64.Lazy as B
import Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding as E
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as EL
import qualified ProtoDB.Types as P
import Text.ProtocolBuffers.Basic
import Treb.Filter
import Treb.Types

default (T.Text)

typeObject = (object .) . (:) . ("type" .=)

instance ToJSON DataBlockName where
    toJSON (AdHocName n user) = typeObject "datablock_name"
            [ "datablock_name_type"    .= AdHocType
            , "datablock_name"         .= n
            , "datablock_name_creator" .= user
            ]
    toJSON (RecipeName c rs p) = typeObject "datablock_name"
            [ "datablock_name_type"      .= RecipeType
            , "datablock_name_compound"  .= c
            , "datablock_name_recipes"   .= rs
            , "datablock_name_pipeline"  .= p
            ]
    toJSON (JobResultName i n) = typeObject "datablock_name"
            [ "datablock_name_type" .= JobResultType
            , "datablock_job_id"    .= i
            , "datablock_name"      .= n
            ]
    toJSON (AliasName n) = typeObject "datablock_name"
            [ "datablock_name_type" .= AliasType
            , "datablock_name" .= n
            ]

instance FromJSON DataBlockName where
    parseJSON (Object v) = do
        "datablock_name" <- v .: "type"
        t <- v .: "datablock_name_type"
        case t of AdHocType     -> AdHocName <$> v .: "datablock_name"
                                             <*> v .: "datablock_name_creator"
                  RecipeType    -> RecipeName <$> v .: "datablock_name_compound"
                                              <*> v .: "datablock_name_recipes"
                                              <*> v .: "datablock_name_pipeline"
                  JobResultType -> JobResultName <$> v .: "datablock_job_id"
                                                 <*> v .: "datablock_name"
                  AliasType     -> AliasName <$> v .: "datablock_name"

instance ToJSON DataBlockNameType where
    toJSON AdHocType     = "ad_hoc"
    toJSON RecipeType    = "recipe"
    toJSON JobResultType = "job_result"
    toJSON AliasType     = "alias"

instance FromJSON DataBlockNameType where
    parseJSON (String "ad_hoc")      = return AdHocType
    parseJSON (String "recipe_type") = return RecipeType
    parseJSON (String "job_result")  = return JobResultType
    parseJSON (String "alias")       = return AliasType
    parseJSON _                      = fail "bad data_block_name_type"

instance ToJSON DataBlockFilter where
    toJSON (NameType t) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "name_type"
            , "datablock_filter_name_type" .= t
            ]
    toJSON (NameExact n) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "name_exact"
            , "datablock_filter_name" .= n
            ]
    toJSON (NameRegex e) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "name_regex"
            , "datablock_filter_name_regex" .= e
            ]
    toJSON (IdExact i) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "id_exact"
            , "datablock_filter_id" .= i
            ]
    toJSON Owned = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "owned" ]
    toJSON (UserIdExact i) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "user_id_exact"
            , "datablock_filter_user_id" .= i
            ]
    toJSON (UserNameExact n) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "user_name_exact"
            , "datablock_filter_user_name" .= n
            ]
    toJSON (UserNameRegex e) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "user_name_regex"
            , "datablock_fitler_user_name_regex" .= e
            ]
    toJSON (RealNameExact n) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "real_name_exact"
            , "datablock_filter_real_name" .= n
            ]
    toJSON (RealNameRegex e) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "real_name_regex"
            , "datablock_fitler_real_name_regex" .= e
            ]
    toJSON (EmailExact a) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "email_exact"
            , "datablock_filter_email" .= a
            ]
    toJSON (EmailRegex e) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "email_regex"
            , "datablock_fitler_email_regex" .= e
            ]
    toJSON (ContainsField f) = typeObject "datablock_filter"
            [ "datablock_filter_op" .= "contains_field"
            , "datablock_fitler_field" .= f
            ]

instance FromJSON DataBlockFilter where
    parseJSON (Object v) = do
        "datablock_filter" <- v .: "type"
        o <- v .: "datablock_filter_op"
        case o of "name_type"       -> NameType <$> v .: "datablock_filter_name_type"
                  "name_exact"      -> NameExact <$> v .: "datablock_filter_name"
                  "name_regex"      -> NameRegex <$> v .: "datablock_filter_name_regex"
                  "hash_exact"      -> IdExact <$> v .: "datablock_filter_hash"
                  "owned"           -> return Owned
                  "user_id_exact"   -> UserIdExact <$> v .: "datablock_filter_user_id"
                  "user_name_exact" -> UserNameExact <$> v .: "datablock_filter_user_name"
                  "user_name_regex" -> UserNameRegex <$> v .: "datablock_filter_user_name_regex"
                  "real_name_exact" -> RealNameExact <$> v .: "datablock_filter_real_name"
                  "real_name_regex" -> RealNameRegex <$> v .: "datablock_filter_real_name_regex"
                  "email_exact"     -> EmailExact <$> v .: "datablock_filter_email"
                  "email_regex"     -> EmailRegex <$> v .: "datablock_filter_email_regex"
                  "contains_field"  -> ContainsField <$> v .: "datablock_filter_field"
                  _                 -> fail "invalid datablock_filter_op"

instance ToJSON DataBlockField where
    toJSON (DataBlockField n t s i m) = typeObject "datablock_field"
            [ "datablock_field_name"         .= n
            , "datablock_field_type"         .= t
            , "datablock_field_vector_shape" .= s
            , "datablock_field_indexed"      .= i
            , "datablock_field_mime_type"    .= m
            ]

instance FromJSON DataBlockField where
    parseJSON (Object v) = do
        "datablock_field" <- v .: "type"
        DataBlockField <$> v .: "datablock_field_name"
                       <*> v .: "datablock_field_type"
                       <*> v .: "datablock_field_vector_shape"
                       <*> v .: "datablock_field_indexed"
                       <*> v .: "datablock_field_mime_type"

-- | Note that this instance encodes only the datablock metadata.
instance ToJSON DataBlock where
    toJSON (DataBlock n i o _ fs _ _ s rs) = typeObject "datablock"
            [ "datablock_name" .= n
            , "datablock_id" .= i
            , "datablock_owner" .= o
            , "datablock_fields" .= fs
            , "datablock_byte_size" .= s
            , "datablock_record_count" .= rs
            ]

instance ToJSON DataBlockRecordFilter where
    toJSON (FieldEq f v) = typeObject "datablock_record_filter"
            [ "datablock_record_filter_op" .= "eq"
            , "datablock_record_filter_field" .= f
            , "datablock_record_filter_value" .= v
            ]
    toJSON (FieldGt f v) = typeObject "datablock_record_filter"
            [ "datablock_record_filter_op" .= "gt"
            , "datablock_record_filter_field" .= f
            , "datablock_record_filter_value" .= v
            ]
    toJSON (FieldGtEq f v) = typeObject "datablock_record_filter"
            [ "datablock_record_filter_op" .= "gteq"
            , "datablock_record_filter_field" .= f
            , "datablock_record_filter_value" .= v
            ]
    toJSON (FieldLt f v) = typeObject "datablock_record_filter"
            [ "datablock_record_filter_op" .= "lt"
            , "datablock_record_filter_field" .= f
            , "datablock_record_filter_value" .= v
            ]
    toJSON (FieldLtEq f v) = typeObject "datablock_record_filter"
            [ "datablock_record_filter_op" .= "lteq"
            , "datablock_record_filter_field" .= f
            , "datablock_record_filter_value" .= v
            ]

instance FromJSON DataBlockRecordFilter where
    parseJSON (Object v) = do
        "datablock_record_filter" <- v .: "type"
        o <- v .: "datablock_record_filter_op"
        case o of "eq"   -> FieldEq <$>
                            v .: "datablock_record_filter_field" <*>
                            v .: "datablock_record_filter_value"
                  "gt"   -> FieldGt <$>
                            v .: "datablock_record_filter_field" <*>
                            v .: "datablock_record_filter_value"
                  "gteq" -> FieldGtEq <$>
                            v .: "datablock_record_filter_field" <*>
                            v .: "datablock_record_filter_value"
                  "lt"   -> FieldLt <$>
                            v .: "datablock_record_filter_field" <*>
                            v .: "datablock_record_filter_value"
                  "lteq" -> FieldLtEq <$>
                            v .: "datablock_record_filter_field" <*>
                            v .: "datablock_record_filter_value"
                  _      -> fail "invalid datablock_record_filter_op"

instance ToJSON Paging where
    toJSON (LinearSampling n) = typeObject "paging"
        [ "paging_type" .= "linear_sampling"
        , "paging_n" .= n
        ]
    toJSON (LinearChunking n) = typeObject "paging"
            [ "paging_type" .= "linear_chunking"
            , "paging_n" .= n
            ]
    toJSON Bisection  = typeObject "paging" ["paging_type" .= "bisection"]
    toJSON Contiguous = typeObject "paging" ["paging_type" .= "contiguous"]

instance FromJSON Paging where
    parseJSON (Object v) = do
        "paging" <- v .: "type"
        t <- v .: "paging_type"
        case t of "linear_sampling" -> LinearSampling <$> v .: "paging_n"
                  "linear_chunking" -> LinearChunking <$> v .: "paging_n"
                  "bisection"       -> return Bisection
                  "contiguous"      -> return Contiguous
                  _                 -> fail "invalid paging_type"

instance ToJSON FieldSelector where
    toJSON (WhiteList fs) = typeObject "field_selector"
            [ "field_selector_op" .= "whitelist"
            , "field_selector_list" .= fs
            ]
    toJSON (BlackList fs) = typeObject "field_selector"
            [ "field_selector_op" .= "blacklist"
            , "field_selector_list" .= fs
            ]

instance FromJSON FieldSelector where
    parseJSON (Object v) = do
        "field_selector" <- v .: "type"
        o <- v .: "field_selector_op"
        case o of "whitelist" -> WhiteList <$> v .: "field_selector_list"
                  "blacklist" -> BlackList <$> v .: "field_selector_list"
                  _           -> fail "invalid field_selector_op"

instance ToJSON Query where
    toJSON (Query n f s l p) = typeObject "query"
            [ "query_datablock_name" .= n
            , "query_filter" .= f
            , "query_sort" .= s
            , "query_list" .= l
            , "query_paging" .= p
            ]

instance FromJSON Query where
    parseJSON (Object v) = do
        "query" <- v .: "type"
        Query <$> v .:  "query_datablock_name"
              <*> v .:  "query_filter"
              <*> v .:? "query_sort"
              <*> v .:? "query_list"
              <*> v .:  "query_paging"

instance ToJSON NewDataBlock where
    toJSON (NewDataBlock n o fs) = typeObject "new_datablock"
            [ "new_datablock_name" .= n
            , "new_datablock_owner" .= o
            , "new_datablock_fields" .= fs
            ]

instance FromJSON NewDataBlock where
    parseJSON (Object v) = do
        "new_datablock" <- v .: "type"
        NewDataBlock <$> v .: "new_datablock_name"
                     <*> v .: "new_datablock_owner"
                     <*> v .: "new_datablock_fields"

instance ToJSON Auth where
    toJSON (Auth u p) = typeObject "auth"
            [ "auth_username" .= u
            , "auth_password" .= p
            ]

instance FromJSON Auth where
    parseJSON (Object v) = do
        "auth" <- v .: "type"
        Auth <$> v .: "auth_username"
             <*> v .: "auth_password"

instance ToJSON User where
    toJSON (User i un rn e) = typeObject "user"
            [ "user_id" .= i
            , "user_name" .= un
            , "user_real_name" .= rn
            , "user_email" .= e
            ]

instance FromJSON User where
    parseJSON (Object v) = do
        "user" <- v .: "type"
        User <$> v .: "user_id"
             <*> v .: "user_name"
             <*> v .: "user_real_name"
             <*> v .: "user_email"

instance ToJSON JobArgType where
    toJSON BoolArgType      = typeObject "job_arg_type" ["job_arg_type" .= "bool"]
    toJSON IntArgType       = typeObject "job_arg_type" ["job_arg_type" .= "int"]
    toJSON RealArgType      = typeObject "job_arg_type" ["job_arg_type" .= "real"]
    toJSON StringArgType    = typeObject "job_arg_type" ["job_arg_type" .= "string"]
    toJSON (EnumArgType vs) = typeObject "job_arg_type"
            [ "job_arg_type" .= "enum"
            , "job_arg_type_enums" .= vs
            ]
    toJSON (RegexArgType rs) = typeObject "job_arg_type"
            [ "job_arg_type" .= "regex"
            , "job_arg_type_regex" .= rs
            ]
    toJSON DataBlockNameArgType = typeObject "job_arg_type"
            [ "job_arg_type" .= "datablock_name" ]
    toJSON DataBlockFieldArgType = typeObject "job_arg_type"
            [ "job_arg_type" .= "datablock_field" ]
    toJSON DataBlockTagArgType = typeObject "job_arg_type"
            [ "job_arg_type" .= "datablock_tag" ]
    toJSON (VectorArgType s t) = typeObject "job_arg_type"
            [ "job_arg_type" .= "vector"
            , "job_arg_type_shape" .= s
            , "job_arg_type_types" .= t
            ]

instance FromJSON JobArgType where
    parseJSON (Object v) = do
        "job_arg_type" <- v .: "job_arg_type"
        t <- v .: "job_arg_type"
        case t of "bool"   -> return BoolArgType
                  "int"    -> return IntArgType
                  "real"   -> return RealArgType
                  "string" -> return StringArgType
                  "enum"   -> EnumArgType <$> v .: "job_arg_type_enums"
                  "regex"  -> RegexArgType <$> v .: "job_arg_type_regex"
                  "datablock_name" -> return DataBlockNameArgType
                  "datablock_field" -> return DataBlockFieldArgType
                  "datablock_tag" -> return DataBlockTagArgType
                  "vector" -> VectorArgType <$> v .:? "job_arg_type_shape"
                                            <*> v .:  "job_arg_type_types"

instance ToJSON JobArg where
    toJSON (BoolArg b) = typeObject "job_arg"
            [ "job_arg_type" .= "bool_arg"
            , "job_arg_value" .= b
            ]
    toJSON (IntArg i) = typeObject "job_arg"
            [ "job_arg_type" .= "int_arg"
            , "job_arg_value" .= i
            ]
    toJSON (RealArg d) = typeObject "job_arg"
            [ "job_arg_type" .= "real_arg"
            , "job_arg_value" .= d
            ]
    toJSON (StringArg s) = typeObject "job_arg"
            [ "job_arg_type" .= "string_arg"
            , "job_arg_value" .= s
            ]
    toJSON (EnumArg s) = typeObject "job_arg"
            [ "job_arg_type" .= "enum_arg"
            , "job_arg_value" .= s
            ]
    toJSON (RegexArg s) = typeObject "job_arg"
            [ "job_arg_type" .= "regex_arg"
            , "job_arg_value" .= s
            ]
    toJSON (DataBlockNameArg n) = typeObject "job_arg"
            [ "job_arg_type" .= "datablock_name_arg"
            , "job_arg_value" .= n
            ]
    toJSON (DataBlockFieldArg n f) = typeObject "job_arg"
            [ "job_arg_type" .= "datablock_field_arg"
            , "job_arg_datablock" .= n
            , "job_arg_field" .= f
            ]
    toJSON (DataBlockTagArg t) = typeObject "job_arg"
            [ "job_arg_type" .= "datablock_tag_arg"
            , "job_arg_value" .= t
            ]
    toJSON (VectorArg v) = typeObject "job_arg"
            [ "job_arg_type" .= "vector_arg"
            , "job_arg_value" .= v
            ]

instance FromJSON JobArg where
    parseJSON (Object v) = do
        "job_arg" <- v .: "type"
        t <- v .: "job_arg_type"
        case t of "bool_arg"            -> BoolArg <$> v .: "job_arg_value"
                  "int_arg"             -> IntArg <$> v .: "job_arg_value"
                  "real_arg"            -> RealArg <$> v .: "job_arg_value"
                  "string_arg"          -> StringArg <$> v .: "job_arg_value"
                  "enum_arg"            -> EnumArg <$> v .: "job_arg_value"
                  "regex_arg"           -> RegexArg <$> v .: "job_arg_value"
                  "datablock_name_arg"  -> DataBlockNameArg <$> v .: "job_arg_value"
                  "datablock_field_arg" -> DataBlockFieldArg <$> v .: "job_arg_datablock"
                                                             <*> v .: "job_arg_field"
                  "datablock_tag_arg"   -> DataBlockTagArg <$> v .: "job_arg_value"
                  "vector_arg"          -> VectorArg <$> v .: "job_arg_value"
                  _                     -> fail "bad job_arg_type in job_arg"

andList :: JobArgVal -> JobArgVal -> [JobArgVal]
andList (JobArgAnd l r) (JobArgAnd l' r') = andList l r ++ andList l' r'
andList (JobArgAnd l r) r'                = andList l r ++ [r']
andList l               (JobArgAnd l' r') = l : andList l' r'
andList l               r                 = [l,r]

orList :: JobArgVal -> JobArgVal -> [JobArgVal]
orList (JobArgOr l r) (JobArgOr l' r') = orList l r ++ orList l' r'
orList (JobArgOr l r) r'               = orList l r ++ [r']
orList l              (JobArgOr l' r') = l : orList l' r'
orList l              r                = [l,r]

xorList :: JobArgVal -> JobArgVal -> [JobArgVal]
xorList (JobArgXor l r) (JobArgXor l' r') = xorList l r ++ xorList l' r'
xorList (JobArgXor l r) r'                = xorList l r ++ [r']
xorList l               (JobArgXor l' r') = l : xorList l' r'
xorList l               r                 = [l,r]

instance ToJSON JobArgVal where
    toJSON (JobArg n) = typeObject "job_arg_val"
            [ "job_arg_val_clause"     .= "atom"
            , "job_arg_val_param_name" .= n
            ]
    toJSON (JobArgNot a) = typeObject "job_arg_val"
            [ "job_arg_val_clause" .= "not"
            , "job_arg_val_not"    .= a
            ]
    toJSON (JobArgAnd l r) = typeObject "job_arg_val"
            [ "job_arg_val_clause" .= "and"
            , "job_arg_val_ands"   .= andList l r
            ]
    toJSON (JobArgOr l r) = typeObject "job_arg_val"
            [ "job_arg_val_clause" .= "or"
            , "job_arg_val_ors"    .= orList l r
            ]
    toJSON (JobArgXor l r) = typeObject "job_arg_val"
            [ "job_arg_val_clause" .= "xor"
            , "job_arg_val_xors"   .= xorList l r
            ]

andUnlist :: [JobArgVal] -> Parser JobArgVal
andUnlist []     = fail "empty job_arg_and list"
andUnlist (x:[]) = return x
andUnlist (x:xs) = JobArgAnd x <$> andUnlist xs

orUnlist :: [JobArgVal] -> Parser JobArgVal
orUnlist []     = fail "empty job_arg_or list"
orUnlist (x:[]) = return x
orUnlist (x:xs) = JobArgOr x <$> orUnlist xs

xorUnlist :: [JobArgVal] -> Parser JobArgVal
xorUnlist []     = fail "empty job_arg_xor list"
xorUnlist (x:[]) = return x
xorUnlist (x:xs) = JobArgXor x <$> xorUnlist xs

instance FromJSON JobArgVal where
    parseJSON (Object v) = do
        "job_arg_val" <- v .: "types"
        c <- v .: "job_arg_val_clause"
        case c of "atom" -> JobArg <$> v .: "job_arg_val_param_name"
                  "not"  -> JobArgNot <$> v .: "job_arg_val_not"
                  "and"  -> v .: "job_arg_val_ands" >>= andUnlist
                  "or"   -> v .: "job_arg_val_ors"  >>= orUnlist
                  "xor"  -> v .: "job_arg_val_xors" >>= xorUnlist
                  _      -> fail "invalid job_arg_val_clause"

instance ToJSON JobParam where
    toJSON (JobParam dn kn de df c at) = typeObject "job_param"
        [ "job_param_disp_name" .= dn
        , "job_param_key_name" .= kn
        , "job_param_desc" .= de
        , "job_param_default" .= df
        , "job_param_category" .= c
        , "job_param_arg_type" .= at
        ]

instance FromJSON JobParam where
    parseJSON (Object v) = do
        "job_param" <- v .: "type"
        JobParam <$> v .:  "job_param_disp_name"
                 <*> v .:  "job_param_key_name"
                 <*> v .:? "job_param_desc"
                 <*> v .:? "job_param_default"
                 <*> v .:  "job_param_category"
                 <*> v .:  "job_param_arg_type"

instance ToJSON JobTemplate where
    toJSON (JobTemplate i n d ps c ts) = typeObject "job_template"
            [ "job_template_id"             .= i
            , "job_template_name"           .= n
            , "job_template_description"    .= d
            , "job_template_params"         .= ps
            , "job_template_constraints"    .= c
            , "job_template_datablock_tags" .= ts
            ]

instance FromJSON JobTemplate where
    parseJSON (Object v) = do
        "job_template" <- v .: "type"
        JobTemplate <$> v .:  "job_template_id"
                    <*> v .:  "job_template_name"
                    <*> v .:? "job_template_description"
                    <*> v .:  "job_template_params"
                    <*> v .:? "job_template_constraints"
                    <*> v .:  "job_template_datablock_tags"

instance ToJSON JobConfig where
    toJSON (JobConfig n t a d) = typeObject "job_config"
            [ "job_config_name"        .= n
            , "job_config_template_id" .= t
            , "job_config_arguments"   .= a
            , "job_config_payload"     .= d
            ]

instance FromJSON JobConfig where
    parseJSON (Object v) = do
        "job_config" <- v .: "type"
        JobConfig <$> v .:? "job_config_name"
                  <*> v .: "job_config_template_id"
                  <*> v .: "job_config_arguments"
                  <*> v .: "job_config_payload"

instance ToJSON JobError where
    toJSON (JobCanceled c t) = typeObject "job_error"
            [ "job_error_type" .= "job_canceled"
            , "job_error_calceler" .= c
            , "job_error_cancel_time" .= t
            ]
    toJSON (JobFailure e t) = typeObject "job_error"
            [ "job_error_type" .= "job_failure"
            , "job_error_string" .= e
            , "job_error_time" .= t
            ]

instance ToJSON Job where
    toJSON (Job i e c s st r) = typeObject "job"
            [ "job_id"               .= i
            , "job_executor"         .= e
            , "job_config"           .= c
            , "job_start_time"       .= s
            , "job_status"           .= st
            , "job_result_datablock" .= r
            ]

instance ToJSON (Filter (M.Map DataBlockName DataBlock)) where
    toJSON (FilterAtom a) = typeObject "filter"
            [ "filter_op" .= "atom"
            , "filter_atom" .= a
            ]
    toJSON (FilterNeg c) = typeObject "filter"
            [ "filter_op" .= "neg"
            , "filter_neg" .= c
            ]
    toJSON (FilterConj l r) = typeObject "filter"
            [ "filter_op" .= "conj"
            , "filter_conj" .= conjList l r
            ]
    toJSON (FilterDisj l r) = typeObject "filter"
            [ "filter_op" .= "disj"
            , "filter_disj" .= disjList l r
            ]

instance ToJSON (Filter RecordReader) where
    toJSON (FilterAtom a) = typeObject "filter"
            [ "filter_op" .= "atom"
            , "filter_atom" .= a
            ]
    toJSON (FilterNeg c) = typeObject "filter"
            [ "filter_op" .= "neg"
            , "filter_neg" .= c
            ]
    toJSON (FilterConj l r) = typeObject "filter"
            [ "filter_op" .= "conj"
            , "filter_conj" .= conjList l r
            ]
    toJSON (FilterDisj l r) = typeObject "filter"
            [ "filter_op" .= "disj"
            , "filter_disj" .= disjList l r
            ]

instance FromJSON (Filter (M.Map DataBlockName DataBlock)) where
    parseJSON (Object v) = do
        "filter" <- v .: "type"
        o <- v .: "filter_op"
        case o of "atom" -> FilterAtom <$> v .: "filter_atom"
                  "neg"  -> FilterNeg <$> v .: "filter_neg"
                  "conj" -> v .: "filter_conj" >>= conjUnlist
                  "disj" -> v .: "filter_disj" >>= disjUnlist
                  _      -> fail "invalid filter_op"

instance FromJSON (Filter RecordReader) where
    parseJSON (Object v) = do
        "filter" <- v .: "type"
        o <- v .: "filter_op"
        case o of "atom" -> FilterAtom <$> v .: "filter_atom"
                  "neg"  -> FilterNeg <$> v .: "filter_neg"
                  "conj" -> v .: "filter_conj" >>= conjUnlist
                  "disj" -> v .: "filter_disj" >>= disjUnlist
                  _      -> fail "invalid filter_op"

instance ToJSON P.ProtoCell where
    toJSON (P.ProtoIntCell i) = typeObject "cell"
            [ "cell_type" .= P.ProtoIntType
            , "cell_value" .= i
            ]
    toJSON (P.ProtoRealCell d) = typeObject "cell"
            [ "cell_type" .= P.ProtoRealType
            , "cell_value" .= d
            ]
    toJSON (P.ProtoStringCell t) = typeObject "cell"
            [ "cell_type" .= P.ProtoStringType
            , "cell_value" .= t
            ]
    toJSON (P.ProtoDateTimeCell i) = typeObject "cell"
            [ "cell_type" .= P.ProtoDateTimeType
            , "cell_value" .= i
            ]
    toJSON (P.ProtoBinaryCell b) = typeObject "cell"
            [ "cell_type" .= P.ProtoBinaryType
            , "cell_value" .= b
            ]

instance FromJSON P.ProtoCell where
    parseJSON (Object v) = do
        "cell" <- v .: "cell"
        t <- v .: "cell_type"
        case t of P.ProtoIntType      -> P.ProtoIntCell <$> v .: "cell_value"
                  P.ProtoRealType     -> P.ProtoRealCell <$> v .: "cell_value"
                  P.ProtoStringType   -> P.ProtoStringCell <$> v .: "cell_value"
                  P.ProtoDateTimeType -> P.ProtoDateTimeCell <$> v .: "cell_value"
                  P.ProtoBinaryType   -> P.ProtoBinaryCell <$> v .: "cell_value"

instance ToJSON P.ProtoInt where
    toJSON (P.ProtoInt (Just i)) = toJSON i
    toJSON (P.ProtoInt Nothing)  = Null

instance FromJSON P.ProtoInt where
    parseJSON i@(Number _) = P.ProtoInt . Just <$> parseJSON i
    parseJSON Null         = return $ P.ProtoInt Nothing

instance ToJSON P.ProtoReal where
    toJSON (P.ProtoReal (Just d)) = toJSON d
    toJSON (P.ProtoReal Nothing)  = Null

instance FromJSON P.ProtoReal where
    parseJSON d@(Number _) = P.ProtoReal . Just <$> parseJSON d
    parseJSON Null         = return $ P.ProtoReal Nothing

instance ToJSON P.ProtoString where
    toJSON (P.ProtoString (Just t)) = String (TL.toStrict (EL.decodeUtf8 (utf8 t)))
    toJSON (P.ProtoString Nothing)  = Null

instance FromJSON P.ProtoString where
    parseJSON (String t) = return $ P.ProtoString (Just (Utf8 (BL.fromStrict (E.encodeUtf8 t))))
    parseJSON Null       = return $ P.ProtoString Nothing

instance ToJSON P.ProtoDateTime where
    toJSON (P.ProtoDateTime (Just i)) = toJSON i
    toJSON (P.ProtoDateTime Nothing)  = Null

instance FromJSON P.ProtoDateTime where
    parseJSON i@(Number _) = P.ProtoDateTime . Just <$> parseJSON i
    parseJSON Null         = return $ P.ProtoDateTime Nothing

instance ToJSON P.ProtoBinary where
    toJSON (P.ProtoBinary (Just b)) = String (TL.toStrict (EL.decodeUtf8 (B.encode b)))
    toJSON (P.ProtoBinary Nothing)  = Null

instance FromJSON P.ProtoBinary where
    parseJSON (String s) = return $ P.ProtoBinary (Just (B.decodeLenient (BL.fromStrict (E.encodeUtf8 s))))
    parseJSON Null        = return $ P.ProtoBinary Nothing

instance ToJSON MIME.Type where
    toJSON = String . MIME.showType

instance FromJSON MIME.Type where
    parseJSON (String mt) = return $ fromJust $ MIME.parseMIMEType mt

instance ToJSON ClientError where
  toJSON (ClientError code msg) =
    object
      [ "type"          .= String "client_error"
      , "error_code"    .= code
      , "error_message" .= msg ]

instance ToJSON ClientErrorCode where
  toJSON CEMissingSessionCookie = String "missing_session_cookie"
  toJSON CEInvalidSessionCookie = String "invalid_session_cookie"
  toJSON CEUserNotFound         = String "user_not_found"
  toJSON CEInvalidCSV           = String "invalid_csv"

instance ToJSON P.ProtoCellType where
    toJSON P.ProtoIntType      = "int"
    toJSON P.ProtoRealType     = "real"
    toJSON P.ProtoStringType   = "string"
    toJSON P.ProtoDateTimeType = "datetime"
    toJSON P.ProtoBinaryType   = "binary"

instance FromJSON P.ProtoCellType where
    parseJSON (String "int")      = return P.ProtoIntType
    parseJSON (String "real")     = return P.ProtoRealType
    parseJSON (String "string")   = return P.ProtoStringType
    parseJSON (String "datetime") = return P.ProtoDateTimeType
    parseJSON (String "binary")   = return P.ProtoBinaryType
