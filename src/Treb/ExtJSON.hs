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
        [ "type"                   .= String "datablock"
        , "datablock_id"           .= datablockId db
        , "datablock_name"         .= datablockName db
        , "datablock_fields"       .= datablockFields db
        , "datablock_record_count" .= datablockRecordCount db
        , "datablock_source"       .= datablockSource db
        , "datablock_records"      .= datablockRecords db ]
          
instance FromJSON DataBlock where
    parseJSON o@(Object v) = do
      "datablock" <- v .: "type" :: Parser Text
      DataBlock <$> v .:  "datablock_id"
                <*> v .:  "datablock_name"
                <*> v .:  "datablock_fields"
                <*> v .:  "datablock_record_count"
                <*> v .:? "datablock_source"
                <*> v .:? "datablock_records"

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
        [ "type"                     .= String "datablock_create"
        , "datablock_create_name"    .= datablockCreateName dbc
        , "datablock_create_fields"  .= datablockCreateFields dbc
        , "datablock_create_records" .= datablockCreateRecords dbc ]
          
instance FromJSON DataBlockCreate where
    parseJSON (Object v) = do
      "datablock_create" <- v .: "type" :: Parser Text
      DataBlockCreate <$> v .:  "datablock_create_name"
                      <*> v .:? "datablock_create_fields"
                      <*> v .:? "datablock_create_records"

instance ToJSON DataBlockField where
    toJSON (DataBlockField name ty indexed) =
      mobject $
        [ "type"                    .= String "datablock_field"
        , "datablock_field_name"    .= name
        , "datablock_field_type"    .= ty
        , "datablock_field_indexed" .= indexed ]
          
instance FromJSON DataBlockField where
    parseJSON (Object v) = do
      "datablock_field" <- v .: "type" :: Parser Text
      DataBlockField <$> v .: "datablock_field_name"
                     <*> v .: "datablock_field_type"
                     <*> v .:? "datablock_field_indexed"

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
        [ "type" .= ("datablock_name" :: Text) ]
        ++ case dbn of
          AdHocName text ->
            [ "datablock_name_type" .= ("ad_hoc" :: Text)
            , "datablock_name_text" .= text ]
          RecipeName c rs ->
            [ "datablock_name_type" .= ("recipe" :: Text)
            , "datablock_name_compound" .= c
            , "datablock_name_recipes" .= rs ]
          JobResultName jobId text ->
            [ "datablock_name_type" .= ("job_result" :: Text)
            , "datablock_name_job_id" .= show jobId
            , "datablock_name_text" .= text ]
          AliasName text ->
            [ "datablock_name_type" .= ("alias" :: Text)
            , "datablock_name_text" .= text ]
          
instance FromJSON DataBlockName where
    parseJSON (Object v) = do
        "datablock_name" <- v .: "type" :: Parser Text
        t <- v .: "datablock_name_type"
        case t of
          AdHocType     ->
            AdHocName <$> v .: "datablock_name_text"
          RecipeType    ->
            RecipeName <$> v .: "datablock_name_compound"
                       <*> v .: "datablock_name_recipes"
          JobResultType ->
            JobResultName <$> fmap read (v .: "datablock_name_job_id") <*> v .: "datablock_name_text"
          AliasType     ->
            AliasName <$> v .: "datablock_name_text"
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
          [ "datablock_source_type"     .= String "api_user"
          , "datablock_source_username" .= username ]
        APIJobSource jobId ->
          [ "datablock_source_type"   .= String "api_job"
          , "datablock_source_job_id" .= jobId ]
        DataPipelineSource sourceId ->
          [ "datablock_source_type"             .= String "data_pipeline"
          , "datablock_source_data_pipeline_id" .= sourceId ]

instance FromJSON DataBlockSource where
  parseJSON (Object o) = do
    "datablock_source" <- o .: "type" :: Parser Text
    ty <- o .: "datablock_source_type" :: Parser Text
    case ty of
      "api_user" ->
        APIUserSource <$>
          o .: "datablock_source_username"
      "api_job" ->
        APIJobSource <$>
          o .: "datablock_source_job_id"
      "data_pipeline" ->
        DataPipelineSource <$>
          o .: "datablock_source_data_pipeline_id"

instance ToJSON Job where
  toJSON j =
    mobject
      [ "type"                 .= String "job"
      , "job_id"               .= jobId j
      , "job_name"             .= jobCreateName jc
      , "job_template_id"      .= jobCreateTemplateId jc
      , "job_owner_username"   .= jobOwnerUsername j
      , "job_status"           .= jobStatus j
      , "job_start_time"       .= jobStartTime j
      , "job_end_time"         .= jobEndTime j
      , "job_input_datablocks" .= [ mobject [ "input_datablock_id"     .= dbId
                                            , "input_datablock_filter" .= dbFilter
                                            , "input_datablock_key"    .= dbKey ] | (dbId, dbKey, dbFilter) <- jobCreateInputDataBlocks jc ]
      , "job_arguments"        .= [ mobject [ "argument_name"  .= name
                                            , "argument_value" .= value ] | (name, value) <- jobCreateArguments jc ] ]
    where
      jc = jobCreate j
instance FromJSON Job where
  parseJSON (Object v) = do
    "job" <- v .: "type" :: Parser Text
    Job <$> v .: "job_id"
        <*> jc
        <*> v .: "job_owner_username"
        <*> v .: "job_status"
        <*> v .: "job_start_time"
        <*> v .: "job_end_time"
    where
      jc = JobCreate <$> v .: "job_name"
                     <*> v .: "job_template_id"
                     <*> (mapM (\(Object u) -> (,,) <$> u .: "input_datablock_id" <*> u .: "input_datablock_key" <*> u .: "input_datablock_filter") =<< v .: "job_input_datablocks")
                     <*> v .: "job_arguments"

instance ToJSON JobStatus where
  toJSON js =
    case js of
      JobSuccess dbId ->
        mobject
          [ "type"            .= String "job_status"
          , "job_status_type" .= String "success"
          , "job_status_success_output_datablock_id" .= dbId ]
      JobFailed r ->
        mobject
          [ "type"                      .= String "job_status"
          , "job_status_type"           .= String "failed"
          , "job_status_failure_reason" .= r ]
      JobCanceled u ->
        mobject
          [ "type"                   .= String "job_status"
          , "job_status_type"        .= String "canceled"
          , "job_status_canceled_by" .= String u ]
      JobRunning ->
        mobject
          [ "type"            .= String "job_status"
          , "job_status_type" .= String "running" ]

instance FromJSON JobStatus where
  parseJSON (Object v) = do
    "job_status" <- v .: "job_status" :: Parser Text
    ty <- v .: "job_status_type" :: Parser Text
    case ty of
      "success"  -> JobSuccess  <$> v .: "job_status_success_output_datablock_id"
      "failed"   -> JobFailed   <$> v .: "job_status_failure_reason"
      "canceled" -> JobCanceled <$> v .: "job_status_canceled_by"
      "running"  -> pure JobRunning

instance ToJSON JobCreate where
  toJSON jc =
    mobject
      [ "type" .= String "job_create"
      , "job_create_name"             .= jobCreateName jc
      , "job_create_template_id"      .= jobCreateTemplateId jc
      , "job_create_input_datablocks" .= [ mobject [ "input_datablock_id"     .= dbId
                                                   , "input_datablock_filter" .= dbFilter
                                                   , "input_datablock_key"    .= dbKey ] | (dbId, dbKey, dbFilter) <- jobCreateInputDataBlocks jc ]
      , "job_create_arguments"        .= [ mobject [ "argument_name"  .= name
                                                   , "argument_value" .= value ] | (name, value) <- jobCreateArguments jc ] ]
instance FromJSON JobCreate where
  parseJSON (Object v) = do
    "job_create" <- v .: "job_create" :: Parser Text
    JobCreate <$> v .: "job_create_name"
              <*> v .: "job_create_template_id"
              <*> (mapM (\(Object u) -> (,,) <$> u .: "input_datablock_id" <*> u .: "input_datablock_key" <*> u .: "input_datablock_filter") =<< v .: "job_create_input_datablocks")
              <*> v .: "job_create_arguments"

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

-- instance FromJSON DataBlockRecordAtom where
--   parseJSON (String s) = return s

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
        mobject [ "type"     .= String "filter_atom"
                , "filter_atom_name"   .= n
                , "filter_atom_match"  .= m ]

instance FromJSON a => FromJSON (FilterQuery a) where
  parseJSON (Object v) =
        (do "filter_operator" <- v .: "type" :: Parser Text
            operator <- v .: "filter_operator" :: Parser Text
            case operator of
              "and" -> FilterQueryAnd <$> v .: "filter_operands"
              "or"  -> FilterQueryOr  <$> v .: "filter_operands"
              "not" -> FilterQueryNot <$> v .: "filter_operand")
    <|> (do "filter_atom" <- v .: "type" :: Parser Text
            FilterQueryAtom <$> v .: "filter_atom_match"
                            <*> v .: "filter_atom_name")
    
instance ToJSON FilterQueryMatch where
  toJSON fqm =
    case fqm of
      FilterQueryMatchEq     x ->
        mobject [ "filter_atom_match_name"  .= String "eq"
                , "filter_atom_match_value" .= x ]
      FilterQueryMatchNeq    x -> 
        mobject [ "filter_atom_match_name"  .= String "neq"    
                , "filter_atom_match_value" .= x ]
      FilterQueryMatchLt     x -> 
        mobject [ "filter_atom_match_name"  .= String "lt"     
                , "filter_atom_match_value" .= x ]
      FilterQueryMatchLte    x ->
        mobject [ "filter_atom_match_name"  .= String  "lte"    
                , "filter_atom_match_value" .= x ]
      FilterQueryMatchGt     x ->
        mobject [ "filter_atom_match_name"  .= String  "gt"     
                , "filter_atom_match_value" .= x ]
      FilterQueryMatchGte    x ->
        mobject [ "filter_atom_match_name"  .= String  "gte"    
                , "filter_atom_match_value" .= x ]
      FilterQueryMatchBool   x ->
        mobject [ "filter_atom_match_name"  .= String  "bool"   
                , "filter_atom_match_value" .= x ]
      FilterQueryMatchString x ->
        mobject [ "filter_atom_match_name"  .= String  "string" 
                , "filter_atom_match_value" .= x ]
      FilterQueryMatchRegex  x ->
        mobject [ "filter_atom_match_name"  .= String  "regex"  
                , "filter_atom_match_value" .= x ]

instance FromJSON FilterQueryMatch where
  parseJSON (Object v) = do
    matchName <- v .: "filter_atom_match_name" :: Parser Text
    case matchName of
      "eq"     -> FilterQueryMatchEq     <$> v .: "filter_atom_match_value"
      "neq"    -> FilterQueryMatchNeq    <$> v .: "filter_atom_match_value"
      "lt"     -> FilterQueryMatchLt     <$> v .: "filter_atom_match_value"
      "lte"    -> FilterQueryMatchLte    <$> v .: "filter_atom_match_value"
      "gt"     -> FilterQueryMatchGt     <$> v .: "filter_atom_match_value"
      "gte"    -> FilterQueryMatchGte    <$> v .: "filter_atom_match_value"
      "bool"   -> FilterQueryMatchBool   <$> v .: "filter_atom_match_value"
      "string" -> FilterQueryMatchString <$> v .: "filter_atom_match_value"
      "regex"  -> FilterQueryMatchRegex  <$> v .: "filter_atom_match_value"

instance ToJSON JobTemplate where
  toJSON jt =
    mobject
      [ "type"                              .= String "job_template"
      , "job_template_id"                   .= jobTemplateId jt
      , "job_template_name"                 .= jobTemplateName jt
      , "job_template_description"          .= jobTemplateDescription jt
      , "job_template_parameters"           .= jobTemplateParameters jt
      , "job_template_input_datablock_keys" .= jobTemplateInputDatablockKeys jt
      , "job_template_parameter_validation"  .= jobTemplateParameterValidation jt ]

instance FromJSON JobTemplate where
  parseJSON (Object v) = do
    "job_template" <- v .: "type" :: Parser Text
    JobTemplate <$> v .: "job_template_id"
                <*> v .: "job_template_name"
                <*> v .: "job_template_description"
                <*> v .: "job_template_parameters"
                <*> v .: "job_template_input_datablock_keys"
                <*> v .: "job_template_parameter_validation"

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
        Array (V.map String $ V.fromList enum)
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
        (JobTemplateParameterTypeRegex <$> v .: "regex")
    <|> (JobTemplateParameterTypeVector <$> v .: "vector_size"
                                        <*> v .: "vector")
  parseJSON a@(Array v) =
    JobTemplateParameterTypeEnum <$> parseJSON a

instance ToJSON JobTemplateParameter where
  toJSON jtp =
    mobject
      [ "type"                               .= String "job_template_parameter"
      , "job_template_parameter_name"        .= jobTemplateParameterName jtp
      , "job_template_parameter_description" .= jobTemplateParameterDescription jtp
      , "job_template_parameter_default"     .= jobTemplateParameterDefault jtp
      , "job_template_parameter_type"        .= jobTemplateParameterType jtp ]

instance FromJSON JobTemplateParameter where
  parseJSON (Object v) = do
    "job_template_parameter" <- v .: "type" :: Parser Text
    JobTemplateParameter <$> v .: "job_template_parameter_name"
                         <*> v .: "job_template_parameter_description"
                         <*> v .: "job_template_parameter_default"
                         <*> v .: "job_template_parameter_type"

instance ToJSON JobParameterValidation where
  toJSON jpv =
    case jpv of
      JobParameterValidationName name ->
        mobject
          [ "type"                          .= String "job_parameter_validation"
          , "job_parameter_validation_type" .= String "parameter_name"
          , "job_parameter_validation_name" .= name ]
      JobParameterValidationAnd vs ->
        mobject
          [ "type"                              .= String "job_parameter_validation"
          , "job_parameter_validation_type"     .= String "operator"
          , "job_parameter_validation_operator" .= String "and"
          , "job_parameter_validation_operands" .= vs ]
      JobParameterValidationOr  vs ->
        mobject
          [ "type"                              .= String "job_parameter_validation"
          , "job_parameter_validation_type"     .= String "operator"
          , "job_parameter_validation_operator" .= String "or"
          , "job_parameter_validation_operands" .= vs ]
      JobParameterValidationNot  v ->
        mobject
          [ "type"                              .= String "job_parameter_validation"
          , "job_parameter_validation_type"     .= String "operator"
          , "job_parameter_validation_operator" .= String "not"
          , "job_parameter_validation_operand"  .= v ]

instance FromJSON JobParameterValidation where
  parseJSON (Object v) = do
    "job_parameter_validation" <- v .: "type" :: Parser Text
    ty <- v .: "job_parameter_validation_type" :: Parser Text
    case ty of
      "parameter_name" ->
        JobParameterValidationName <$> v .: "job_parameter_validation_name"
      "operator" -> do
        ty' <- v .: "job_parameter_validation_type" :: Parser Text
        case ty' of
          "and" ->
            JobParameterValidationAnd <$> v .: "job_parameter_validation_operands"
          "or"  ->
            JobParameterValidationOr <$> v .: "job_parameter_validation_operands"
          "not" ->
            JobParameterValidationNot <$> v .: "job_parameter_validation_operand"

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
      [ "type"                        .= String "datablock_creation"
      , "datablock_creation_file_url" .= datablockCreationResponseFileURL r
      , "datablock_creation_timeout"  .= formatISO8601 (datablockCreationResponseTimeout r) ]

instance FromJSON DataBlockCreationResponse where
  parseJSON (Object v) = do
    "datablock_creation" <- v .: "type" :: Parser Text
    creationTimeout <- parseISO8601 <$> v .: "datablock_creation_timeout"
    DataBlockCreationResponse <$> v .: "datablock_creation_file_url"
                              <*> pure (maybe (error "ISO 8601 date-time parse failed") (\time -> time) creationTimeout)

instance ToJSON User where
  toJSON u =
    mobject
      [ "type" .= String "user"
      , "user_username" .= userUsername u
      , "user_name" .= userName u
      , "user_email" .= userEmail u
      , "user_datablocks" .= userDataBlocks u ]

instance FromJSON User where
  parseJSON (Object v) = do
    "user" <- v .: "type" :: Parser Text
    User <$> v .: "user_username"
         <*> v .: "user_name"
         <*> v .: "user_email"
         <*> v .: "user_datablocks"

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
      , "data_pipeline_id" .= dataPipelineId dp
      , "data_pipeline_name" .= dataPipelineName dp ]

instance FromJSON DataPipeline where
  parseJSON (Object v) = do
    "data_pipeline" <- v .: "type" :: Parser Text
    DataPipeline <$> v .: "data_pipeline_id"
                 <*> v .: "data_pipeline_name"

instance ToJSON DataBlockRecordAtom where
  toJSON (DataBlockRecordAtom a) = String a

instance FromJSON DataBlockRecordAtom where
  parseJSON (String s) = DataBlockRecordAtom <$> pure s
