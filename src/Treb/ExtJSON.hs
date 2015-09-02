{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Treb.ExtJSON () where

import Data.Aeson
import Treb.ExtTypes
import Data.Time.Clock
import Data.Time.ISO8601
import Control.Applicative

import Data.Aeson.Types (Parser, Pair)
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Vector as V
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME

mobject :: [Pair] -> Value
mobject = object . filter ((/= Null) . snd)

instance ToJSON CASAuth where
  toJSON ca =
    mobject
      [ "type"       .= String "cas_auth"
      , "cas_ticket" .= casAuthTicket ca ]

instance FromJSON CASAuth where
  parseJSON (Object v) = do
    "cas_auth" <- v .: "type" :: Parser Text
    CASAuth <$> v .: "cas_ticket"

instance ToJSON DataBlock where
    toJSON db =
      mobject
        [ "type"         .= String "datablock"
        , "id"           .= datablockId db
        , "name"         .= datablockName db
        , "fields"       .= datablockFields db
        , "record_count" .= datablockRecordCount db
        , "source"       .= datablockSource db
        , "records"      .= datablockRecords db ]
          
instance FromJSON DataBlock where
    parseJSON o@(Object v) = do
      "datablock" <- v .: "type" :: Parser Text
      DataBlock <$> v .:  "id"
                <*> v .:  "name"
                <*> v .:  "fields"
                <*> v .:  "record_count"
                <*> v .:? "source"
                <*> v .:? "records"

instance ToJSON DataBlockCell where
  toJSON cell =
    case cell of
      DataBlockCellBool   b -> toJSON b
      DataBlockCellNumber n -> toJSON n
      DataBlockCellString s -> toJSON s
      DataBlockCellVector v -> toJSON v

instance FromJSON DataBlockCell where
  parseJSON v =
    case v of
      Bool   b -> return $ DataBlockCellBool   b
      Number n -> return $ DataBlockCellNumber n
      String s -> return $ DataBlockCellString s
      Array  a -> DataBlockCellVector <$> V.mapM parseJSON a

instance ToJSON DataBlockCreate where
    toJSON dbc =
      mobject
        [ "type"    .= String "datablock_create"
        , "name"    .= datablockCreateName dbc
        , "fields"  .= datablockCreateFields dbc
        , "records" .= datablockCreateRecords dbc ]
          
instance FromJSON DataBlockCreate where
    parseJSON (Object v) = do
      "datablock_create" <- v .: "type" :: Parser Text
      DataBlockCreate <$> v .:  "name"
                      <*> v .:? "fields"
                      <*> v .:? "records"

instance ToJSON DataBlockField where
    toJSON (DataBlockField name ty indexed) =
      mobject $
        [ "type"     .= String "datablock_field"
        , "name"     .= name
        , "datatype" .= ty
        , "indexed"  .= indexed ]
          
instance FromJSON DataBlockField where
    parseJSON (Object v) = do
      "datablock_field" <- v .: "type" :: Parser Text
      DataBlockField <$> v .:  "name"
                     <*> v .:  "datatype"
                     <*> v .:? "indexed"

instance ToJSON DataBlockFieldType where
    toJSON dbft =
      case dbft of
        DBInt ->
          String "int"
        DBReal ->
          String "real"
        DBString ->
          String "string"
        DBDateTime ->
          String "date_time"
        DBBinary mime ->
          object [ "binary" .= mime ]
        DBVector size ft ->
          object [ "vector"      .= ft
                 , "vector_size" .= size ]
          
instance FromJSON DataBlockFieldType where
    parseJSON (String dbftStr) =
      return $ case dbftStr of
        "int"       -> DBInt
        "real"      -> DBReal
        "string"    -> DBString
        "date_time" -> DBDateTime
    parseJSON (Object v) =
          DBBinary <$> v .: "binary"
      <|> DBVector <$> v .: "vector_size"
                   <*> v .: "vector"

instance ToJSON DataBlockName where
    toJSON dbn =
      mobject $
        [ "type" .= String "datablock_name" ]
        ++ case dbn of
          AdHocName text ->
            [ "datatype" .= String "ad_hoc"
            , "text"     .= text ]
          RecipeName c rs ->
            [ "datatype" .= String "recipe"
            , "compound" .= c
            , "recipes"  .= rs ]
          JobResultName jobId text ->
            [ "datatype" .= String "job_result"
            , "job_id"   .= show jobId
            , "text"     .= text ]
          AliasName text ->
            [ "datatype" .= String "alias"
            , "text"     .= text ]
          
instance FromJSON DataBlockName where
    parseJSON (Object v) = do
        "datablock_name" <- v .: "type" :: Parser Text
        t <- v .: "datatype"
        case t of
          AdHocType     ->
            AdHocName <$> v .: "text"
          RecipeType    ->
            RecipeName <$> v .: "compound"
                       <*> v .: "recipes"
          JobResultType ->
            JobResultName <$> fmap read (v .: "job_id") <*> v .: "text"
          AliasType     ->
            AliasName <$> v .: "text"
instance ToJSON DataBlockNameType where
    toJSON dbnt =
      String $ case dbnt of
        AdHocType     -> "ad_hoc"
        RecipeType    -> "recipe"
        JobResultType -> "job_result"
        AliasType     -> "alias"
          
instance FromJSON DataBlockNameType where
    parseJSON (String dbntStr) =
        return $ case dbntStr of
          "ad_hoc"     -> AdHocType
          "recipe"     -> RecipeType
          "job_result" -> JobResultType
          "alias"      -> AliasType

instance ToJSON DataBlockSource where
  toJSON src =
    mobject $
      [ "type" .= String "datablock_source" ]
      ++ case src of
        APIUserSource username ->
          [ "datatype" .= String "api_user"
          , "username" .= username ]
        APIJobSource jobId ->
          [ "datatype" .= String "api_job"
          , "job_id"   .= jobId ]
        DataPipelineSource sourceId ->
          [ "datatype"         .= String "data_pipeline"
          , "data_pipeline_id" .= sourceId ]

instance FromJSON DataBlockSource where
  parseJSON (Object o) = do
    "datablock_source" <- o .: "type" :: Parser Text
    ty <- o .: "datatype" :: Parser Text
    case ty of
      "api_user" ->
        APIUserSource <$>
          o .: "username"
      "api_job" ->
        APIJobSource <$>
          o .: "job_id"
      "data_pipeline" ->
        DataPipelineSource <$>
          o .: "data_pipeline_id"

instance ToJSON Job where
  toJSON j =
    mobject
      [ "type"             .= String "job"
      , "id"               .= jobId j
      , "name"             .= jobCreateName jc
      , "template_id"      .= jobCreateTemplateId jc
      , "owner_username"   .= jobOwnerUsername j
      , "status"           .= jobStatus j
      , "start_time"       .= jobStartTime j
      , "end_time"         .= jobEndTime j
      , "input_datablocks" .= inputDatablockSetToJson jc
      , "arguments"        .= argumentSetToJson jc ]
    where
      jc = jobCreate j

instance FromJSON Job where
  parseJSON (Object v) = do
    "job" <- v .: "type" :: Parser Text
    Job <$> v .: "id"
        <*> jc
        <*> v .: "owner_username"
        <*> v .: "status"
        <*> v .: "start_time"
        <*> v .: "end_time"
    where
      jc = JobCreate <$> v .: "name"
                     <*> v .: "template_id"
                     <*> (v .: "input_datablocks" >>= mapM parseInputDatablock)
                     <*> (v .: "arguments" >>= mapM parseArgument)

parseArgument :: Value -> Parser (Text, JobArgument)
parseArgument (Object v) =
        (,) <$> v .: "name"
            <*> v .: "value"

parseInputDatablock :: Value -> Parser (DataBlockId, Maybe Text, Maybe DataBlockRecordFilter)
parseInputDatablock (Object v) = 
        (,,) <$> v .: "datablock_id"
             <*> v .: "datablock_key"
             <*> v .: "datablock_filter"

inputDatablockSetToJson :: JobCreate -> [Value]
inputDatablockSetToJson jc = [ mobject [ "datablock_id"     .= dbId
                                       , "datablock_filter" .= dbFilter
                                       , "datablock_key"    .= dbKey ]
                                       | (dbId, dbKey, dbFilter) <- jobCreateInputDataBlocks jc ]

argumentSetToJson :: JobCreate -> [Value]
argumentSetToJson jc = [ mobject [ "name"  .= name
                                 , "value" .= value ]
                                 | (name, value) <- jobCreateArguments jc ]

instance ToJSON JobStatus where
  toJSON js =
    case js of
      JobSuccess dbId ->
        mobject
          [ "type"                .= String "job_status"
          , "datatype"            .= String "success"
          , "output_datablock_id" .= dbId ]
      JobFailed r ->
        mobject
          [ "type"           .= String "job_status"
          , "datatype"       .= String "failed"
          , "failure_reason" .= r ]
      JobCanceled u ->
        mobject
          [ "type"        .= String "job_status"
          , "datatype"    .= String "canceled"
          , "canceled_by" .= String u ]
      JobRunning ->
        mobject
          [ "type"     .= String "job_status"
          , "datatype" .= String "running" ]

instance FromJSON JobStatus where
  parseJSON (Object v) = do
    "job_status" <- v .: "job_status" :: Parser Text
    ty <- v .: "datatype" :: Parser Text
    case ty of
      "success"  -> JobSuccess  <$> v .: "output_datablock_id"
      "failed"   -> JobFailed   <$> v .: "failure_reason"
      "canceled" -> JobCanceled <$> v .: "canceled_by"
      "running"  -> pure JobRunning

instance ToJSON JobCreate where
  toJSON jc =
    mobject
      [ "type"             .= String "job_create"
      , "name"             .= jobCreateName jc
      , "template_id"      .= jobCreateTemplateId jc
      , "input_datablocks" .= inputDatablockSetToJson jc
      , "arguments"        .= argumentSetToJson jc ]

instance FromJSON JobCreate where
  parseJSON (Object v) = do
    "job_create" <- v .: "job_create" :: Parser Text
    JobCreate <$> v .: "name"
              <*> v .: "template_id"
              <*> (v .: "input_datablocks" >>= mapM parseInputDatablock)
              <*> (v .: "arguments" >>= mapM parseArgument)

instance ToJSON JobArgument where
  toJSON ja =
    case ja of
      JobArgumentBool b   -> Bool b
      JobArgumentString s -> String s
      JobArgumentNumber n -> Number n
      JobArgumentVector v -> Array (V.map toJSON v)

instance FromJSON JobArgument where
  parseJSON v =
    case v of
      Bool   b -> JobArgumentBool   <$> pure b   
      String s -> JobArgumentString <$> pure s 
      Number n -> JobArgumentNumber <$> pure n 
      Array  v -> JobArgumentVector <$> V.mapM parseJSON v

instance ToJSON a => ToJSON (FilterQuery a) where
  toJSON fq =
    case fq of
      FilterQueryAnd qs ->
        mobject [ "type"     .= String "filter_operator"
                , "operator" .= String "and"
                , "operands" .= qs ]
      FilterQueryOr qs ->
        mobject [ "type"     .= String "filter_operator"
                , "operator" .= String "or"
                , "operands" .= qs ]
      FilterQueryNot q ->
        mobject [ "type"     .= String "filter_operator"
                , "operator" .= String "not"
                , "operand"  .= q ]
      FilterQueryAtom m n -> 
        mobject [ "type"       .= String "filter_atom"
                , "atom_name"  .= n
                , "atom_match" .= m ]

instance FromJSON a => FromJSON (FilterQuery a) where
  parseJSON (Object v) =
        (do "filter_operator" <- v .: "type" :: Parser Text
            operator <- v .: "operator" :: Parser Text
            case operator of
              "and" -> FilterQueryAnd <$> v .: "operands"
              "or"  -> FilterQueryOr  <$> v .: "operands"
              "not" -> FilterQueryNot <$> v .: "operand")
    <|> (do "filter_atom" <- v .: "type" :: Parser Text
            FilterQueryAtom <$> v .: "atom_match"
                            <*> v .: "atom_name")
    
instance ToJSON FilterQueryMatch where
  toJSON fqm =
    case fqm of
      FilterQueryMatchEq     x ->
        mobject [ "match_name"  .= String "eq"
                , "match_value" .= x ]
      FilterQueryMatchNeq    x -> 
        mobject [ "match_name"  .= String "neq"    
                , "match_value" .= x ]
      FilterQueryMatchLt     x -> 
        mobject [ "match_name"  .= String "lt"     
                , "match_value" .= x ]
      FilterQueryMatchLte    x ->
        mobject [ "match_name"  .= String  "lte"    
                , "match_value" .= x ]
      FilterQueryMatchGt     x ->
        mobject [ "match_name"  .= String  "gt"     
                , "match_value" .= x ]
      FilterQueryMatchGte    x ->
        mobject [ "match_name"  .= String  "gte"    
                , "match_value" .= x ]
      FilterQueryMatchBool   x ->
        mobject [ "match_name"  .= String  "bool"   
                , "match_value" .= x ]
      FilterQueryMatchString x ->
        mobject [ "match_name"  .= String  "string" 
                , "match_value" .= x ]
      FilterQueryMatchRegex  x ->
        mobject [ "match_name"  .= String  "regex"  
                , "match_value" .= x ]

instance FromJSON FilterQueryMatch where
  parseJSON (Object v) = do
    matchName <- v .: "match_name" :: Parser Text
    case matchName of
      "eq"     -> FilterQueryMatchEq     <$> v .: "match_value"
      "neq"    -> FilterQueryMatchNeq    <$> v .: "match_value"
      "lt"     -> FilterQueryMatchLt     <$> v .: "match_value"
      "lte"    -> FilterQueryMatchLte    <$> v .: "match_value"
      "gt"     -> FilterQueryMatchGt     <$> v .: "match_value"
      "gte"    -> FilterQueryMatchGte    <$> v .: "match_value"
      "bool"   -> FilterQueryMatchBool   <$> v .: "match_value"
      "string" -> FilterQueryMatchString <$> v .: "match_value"
      "regex"  -> FilterQueryMatchRegex  <$> v .: "match_value"

instance ToJSON JobTemplate where
  toJSON jt =
    mobject
      [ "type"                 .= String "job_template"
      , "id"                   .= jobTemplateId jt
      , "name"                 .= jobTemplateName jt
      , "description"          .= jobTemplateDescription jt
      , "parameters"           .= jobTemplateParameters jt
      , "input_datablock_keys" .= jobTemplateInputDatablockKeys jt
      , "parameter_validation" .= jobTemplateParameterValidation jt ]

instance FromJSON JobTemplate where
  parseJSON (Object v) = do
    "job_template" <- v .: "type" :: Parser Text
    JobTemplate <$> v .: "id"
                <*> v .: "name"
                <*> v .: "description"
                <*> v .: "parameters"
                <*> v .: "input_datablock_keys"
                <*> v .: "parameter_validation"

instance ToJSON JobTemplateParameterType where
  toJSON jtpt =
    case jtpt of
      JobTemplateParameterTypeBool               -> String "bool"
      JobTemplateParameterTypeDateTime           -> String "date_time"
      JobTemplateParameterTypeInt                -> String "int"
      JobTemplateParameterTypeReal               -> String "real"
      JobTemplateParameterTypeString             -> String "string"
      JobTemplateParameterTypeDataBlockName      -> String "datablock"
      JobTemplateParameterTypeDataBlockFieldName -> String "datablock_field"
      JobTemplateParameterTypeDataBlockKey       -> String "datablock_key"
      JobTemplateParameterTypeEnum enum ->
        mobject
          [ "enum" .= Array (V.map String $ V.fromList enum) ]
      JobTemplateParameterTypeRegex regex -> 
        mobject
          [ "regex" .= regex ]
      JobTemplateParameterTypeVector size ty ->
        mobject
          [ "vector" .= ty
          , "vector_size" .= size ]
      
instance FromJSON JobTemplateParameterType where
  parseJSON (String s) =
    pure $ case s of
      "bool"            -> JobTemplateParameterTypeBool
      "date_time"       -> JobTemplateParameterTypeDateTime
      "int"             -> JobTemplateParameterTypeInt
      "real"            -> JobTemplateParameterTypeReal 
      "string"          -> JobTemplateParameterTypeString
      "datablock"       -> JobTemplateParameterTypeDataBlockName
      "datablock_field" -> JobTemplateParameterTypeDataBlockFieldName
      "datablock_key"   -> JobTemplateParameterTypeDataBlockKey
  parseJSON (Object v) =
        (JobTemplateParameterTypeEnum <$> v .: "enum")
    <|> (JobTemplateParameterTypeRegex <$> v .: "regex")
    <|> (JobTemplateParameterTypeVector <$> v .: "vector_size"
                                        <*> v .: "vector")
  parseJSON a@(Array v) =
    JobTemplateParameterTypeEnum <$> parseJSON a

instance ToJSON JobTemplateParameter where
  toJSON jtp =
    mobject
      [ "type"        .= String "job_template_parameter"
      , "name"        .= jobTemplateParameterName jtp
      , "description" .= jobTemplateParameterDescription jtp
      , "default"     .= jobTemplateParameterDefault jtp
      , "datatype"    .= jobTemplateParameterType jtp ]

instance FromJSON JobTemplateParameter where
  parseJSON (Object v) = do
    "job_template_parameter" <- v .: "type" :: Parser Text
    JobTemplateParameter <$> v .: "name"
                         <*> v .: "description"
                         <*> v .: "default"
                         <*> v .: "datatype"

instance ToJSON JobParameterValidation where
  toJSON jpv =
    case jpv of
      JobParameterValidationName name ->
        mobject
          [ "type"     .= String "job_parameter_validation"
          , "datatype" .= String "parameter_name"
          , "name"     .= name ]
      JobParameterValidationAnd vs ->
        mobject
          [ "type"     .= String "job_parameter_validation"
          , "datatype" .= String "operator"
          , "operator" .= String "and"
          , "operands" .= vs ]
      JobParameterValidationOr  vs ->
        mobject
          [ "type"     .= String "job_parameter_validation"
          , "datatype" .= String "operator"
          , "operator" .= String "or"
          , "operands" .= vs ]
      JobParameterValidationNot  v ->
        mobject
          [ "type"     .= String "job_parameter_validation"
          , "datatype" .= String "operator"
          , "operator" .= String "not"
          , "operand"  .= v ]

instance FromJSON JobParameterValidation where
  parseJSON (Object v) = do
    "job_parameter_validation" <- v .: "type" :: Parser Text
    ty <- v .: "datatype" :: Parser Text
    case ty of
      "parameter_name" ->
        JobParameterValidationName <$> v .: "name"
      "operator" -> do
        op <- v .: "operator" :: Parser Text
        case op of
          "and" -> JobParameterValidationAnd <$> v .: "operands"
          "or"  -> JobParameterValidationOr  <$> v .: "operands"
          "not" -> JobParameterValidationNot <$> v .: "operand"

instance ToJSON (Maybe MIME.Type) where
    toJSON = maybe Null String . (>>= Just . MIME.showType)

instance FromJSON (Maybe MIME.Type) where
    -- Keep in mind that a parse failure is distinct from a successful parse returning Nothing.
    -- In the former case: decode ... = Nothing
    -- In the latter case: decode ... = Just Nothing
    --
    -- For a Maybe MIME.Type as JSON, nulls are valid (indicating an unknown MIME Type) and
    -- properly formatted strings are valid. Invalid strings should not be taken for an
    -- explicitly unknown MIME Type. They should be treated as errors.
    parseJSON Null = return Nothing
    parseJSON (String s) = maybe (fail "parseMIMEType") (return . Just) $ MIME.parseMIMEType s

instance ToJSON DataBlockCreationResponse where
  toJSON r =
    mobject
      [ "type"     .= String "datablock_creation"
      , "file_url" .= datablockCreationResponseFileURL r
      , "timeout"  .= formatISO8601 (datablockCreationResponseTimeout r) ]

instance FromJSON DataBlockCreationResponse where
  parseJSON (Object v) = do
    "datablock_creation" <- v .: "type" :: Parser Text
    creationTimeout <- parseISO8601 <$> v .: "timeout"
    DataBlockCreationResponse <$> v .: "file_url"
                              <*> pure (maybe (error "ISO 8601 date-time parse failed") id creationTimeout)

instance ToJSON User where
  toJSON u =
    mobject
      [ "type"       .= String "user"
      , "username"   .= userUsername u
      , "name"       .= userName u
      , "email"      .= userEmail u
      , "datablocks" .= userDataBlocks u ]

instance FromJSON User where
  parseJSON (Object v) = do
    "user" <- v .: "type" :: Parser Text
    User <$> v .: "username"
         <*> v .: "name"
         <*> v .: "email"
         <*> v .: "datablocks"

instance ToJSON DataBlockSetAtom where
  toJSON a =
    case a of
      DataBlockId            -> "datablock_id"
      DataBlockNameType      -> "datablock_name_type"
      DataBlockName          -> "datablock_name"
      DataBlockContainsField -> "datablock_contains_field"
      DataBlockOwned         -> "datablock_owned"
      DataBlockOwnerUsername -> "datablock_owner_username"
      DataBlockOwnerName     -> "datablock_owner_name"
      DataBlockOwnerEmail    -> "datablock_owner_email"

instance FromJSON DataBlockSetAtom where
  parseJSON (String s) =
    return $ case s of
      "datablock_id"             -> DataBlockId
      "datablock_name_type"      -> DataBlockNameType
      "datablock_name"           -> DataBlockName
      "datablock_contains_field" -> DataBlockContainsField
      "datablock_owned"          -> DataBlockOwned
      "datablock_owner_username" -> DataBlockOwnerUsername
      "datablock_owner_name"     -> DataBlockOwnerName
      "datablock_owner_email"    -> DataBlockOwnerEmail

instance ToJSON JobSetAtom where
  toJSON a =
    case a of
      JobId                -> "job_id"
      JobName              -> "job_name"
      JobOwnerUsername     -> "job_owner_username"
      JobOwnerName         -> "job_owner_name"
      JobOwnerEmail        -> "job_owner_email"
      JobTemplateId        -> "job_template_id"
      JobTemplateName      -> "job_template_name"
      JobStatus            -> "job_status"
      JobStartTime         -> "job_start_time"
      JobEndTime           -> "job_end_time"
      JobDuration          -> "job_duration"
      JobInputDataBlockId  -> "job_input_datablock_id"
      JobOutputDataBlockId -> "job_output_datablock_id"

instance FromJSON JobSetAtom where
  parseJSON (String s) =
    return $ case s of
      "job_id"                  -> JobId
      "job_name"                -> JobName
      "job_owner_username"      -> JobOwnerUsername
      "job_owner_name"          -> JobOwnerName
      "job_owner_email"         -> JobOwnerEmail
      "job_template_id"         -> JobTemplateId
      "job_template_name"       -> JobTemplateName
      "job_status"              -> JobStatus
      "job_start_time"          -> JobStartTime
      "job_end_time"            -> JobEndTime
      "job_duration"            -> JobDuration
      "job_input_datablock_id"  -> JobInputDataBlockId
      "job_output_datablock_id" -> JobOutputDataBlockId

instance ToJSON DataPipeline where
  toJSON dp =
    mobject
      [ "type" .= String "data_pipeline"
      , "id" .= dataPipelineId dp
      , "name" .= dataPipelineName dp ]

instance FromJSON DataPipeline where
  parseJSON (Object v) = do
    "data_pipeline" <- v .: "type" :: Parser Text
    DataPipeline <$> v .: "data_pipeline_id"
                 <*> v .: "data_pipeline_name"

instance ToJSON DataBlockRecordAtom where
  toJSON (DataBlockRecordAtom a) = String a

instance FromJSON DataBlockRecordAtom where
  parseJSON (String s) = DataBlockRecordAtom <$> pure s
