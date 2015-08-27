{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Treb.Test.Schema where

-- General
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Data.Aeson (Value(String))
import qualified Data.Aeson as Aeson


-- JSON Schema
import Data.JSON.Schema
import Data.JSON.Schema.Combinators
import Data.JSON.Schema.Validate

import Treb.ExtTypes

typeField :: Text -> Field
typeField = requiredField "type" . Constant . Aeson.String 

typedObj :: Text -> Text -> SchemaC
typedObj ns ty = addFields
  [ ("type", True, Constant $ String ns)
  , (ns <> "_type", True, Constant $ String ty) ]

prependNS :: Text -> Field -> Field
prependNS ns (Field a b c) = Field (ns <> "_" <> a) b c

typedNamespacedObj :: Text -> Text -> [Field] -> Schema
typedNamespacedObj ty ns xs = Object $ (Field "type" True $ Constant (String ty)):(f xs)
  where
    f = map (\(Field key required content) -> Field (ns <> "_" <> key) required content)

typedTypedObj :: Text -> Text -> [Field] -> Schema
typedTypedObj ns tyB xs = Object $ (Field "type" True $ Constant (String ns)):(Field (ns <> "_type") True $ Constant (String tyB)):(f xs)
  where
    f = map (\(Field key required content) -> Field (ns <> "_" <> key) required content)

nonemptyLength :: LengthBound
nonemptyLength = LengthBound (Just 1) Nothing

unboundedString :: Schema
unboundedString = Value unboundedLength

nonemptyString :: Schema
nonemptyString = Value nonemptyLength

constantString :: Text -> Schema
constantString = Constant . String

unboundedArray :: Bool -> Schema -> Schema
unboundedArray = Array unboundedLength

nonemptyArray :: Bool -> Schema -> Schema
nonemptyArray = Array nonemptyLength

unboundedNumber :: Schema
unboundedNumber = Number unbounded

requiredField :: Text -> Schema -> Field
requiredField key content = Field key True content

optionalField :: Text -> Schema -> Field
optionalField key content = Field key False content

enumString :: [Text] -> Schema
enumString = Choice . map constantString

positiveNumber :: Schema
positiveNumber = Number (Bound (Just 0) Nothing)

instance JSONSchema DataBlock where
  schema _ =
    Object
      [ requiredField "datablock_id"             $ nonemptyString
      , requiredField "datablock_name"           $ datablockNameSchema
      , requiredField "datablock_owner_username" $ nonemptyString
      , requiredField "datablock_fields"         $ datablockFieldSetSchema
      , requiredField "datablock_record_count"   $ unboundedNumber
      , optionalField "datablock_source"         $ datablockSourceSchema
      , optionalField "datablock_records"        $ datablockRecordSetSchema ]

datablockSchema :: Schema
datablockSchema = schema (Proxy :: Proxy DataBlock)

instance JSONSchema DataBlockCell where
  schema _ =
        Boolean
    <|> unboundedNumber
    <|> unboundedString
    <|> unboundedArray False (schema (Proxy :: Proxy DataBlockCell))

datablockCellSchema :: Schema
datablockCellSchema = schema (Proxy :: Proxy DataBlockCell)

datablockRecordSchema :: Schema
datablockRecordSchema = unboundedArray False datablockCellSchema

-- | No corresponding JSONSchema instance or type alias.
datablockRecordSetSchema :: Schema 
datablockRecordSetSchema = unboundedArray False datablockRecordSchema

instance JSONSchema DataBlockField where
  schema = const $ Object
    [ requiredField "type" $ Constant $ String "datablock_field"
    , requiredField "datablock_field_name" nonemptyString
    , requiredField "datablock_field_type" datablockFieldTypeSchema
    , optionalField "datablock_field_indexed" Boolean ]

datablockFieldSchema :: Schema
datablockFieldSchema = schema (Proxy :: Proxy DataBlockField)

-- | No corresponding JSONSchema instance or type alias.
datablockFieldSetSchema :: Schema
datablockFieldSetSchema = unboundedArray True datablockFieldSchema

instance JSONSchema DataBlockFieldType where
  schema _ =
        constantString "int"
    <|> constantString "real"
    <|> constantString "string"
    <|> Object [ requiredField "binary" $ nonemptyString <|> Constant Aeson.Null ]
    <|> Object [ requiredField "vector" $ schema (Proxy :: Proxy DataBlockFieldType) ]

datablockFieldTypeSchema :: Schema
datablockFieldTypeSchema = schema (Proxy :: Proxy DataBlockFieldType)

instance JSONSchema DataBlockName where
  schema _ =
        dbnObj "ad_hoc"
          [ requiredField "text" nonemptyString ]
    <|> dbnObj "recipe"
          [ requiredField "compound" nonemptyString
          , requiredField "recipes" (unboundedArray True nonemptyString) ]
    <|> dbnObj "job_result"
          [ requiredField "job_id" nonemptyString
          , requiredField "text"   nonemptyString ]
    <|> dbnObj "alias"
          [ requiredField "text" nonemptyString ]
    where
      ns :: Text
      ns = "datablock_name"

      dbnObj :: Text -> [Field] -> Schema
      dbnObj ty = typedObj ns ty . Object . map (prependNS ns)

datablockNameSchema :: Schema
datablockNameSchema = schema (Proxy :: Proxy DataBlockName)

instance JSONSchema DataBlockSource where
  schema _ =
        obj "api_user"
          [ requiredField "datablock_source_username" nonemptyString ]
    <|> obj "api_job"
          [ requiredField "datablock_source_job_id" nonemptyString ]
    <|> obj "data_pipeline"
          [ requiredField "datablock_source_data_pipeline_id" nonemptyString ]
    where
      obj t rest = 
        Object $ 
          [ requiredField "type" $ Constant $ Aeson.String "datablock_source"
          , requiredField "datablock_source_type" $ Constant $ Aeson.String t ] ++ rest

datablockSourceSchema :: Schema
datablockSourceSchema = schema (Proxy :: Proxy DataBlockSource)

instance JSONSchema CASAuth where
  schema _ =
    Object
      [ typeField "cas_auth"
      , requiredField "cas_ticket" nonemptyString ]

casAuthSchema :: Schema
casAuthSchema = schema (Proxy :: Proxy CASAuth)

instance JSONSchema DataBlockCreate where
  schema _ =
    Object
      [ typeField "datablock_create"
      , requiredField "datablock_create_name" nonemptyString
      , optionalField "datablock_create_fields" (unboundedArray True (schema (Proxy :: Proxy DataBlockField)))
      , optionalField "datablock_create_records" (unboundedArray False (schema (Proxy :: Proxy DataBlockRecord))) ]

datablockCreateSchema :: Schema
datablockCreateSchema = schema (Proxy :: Proxy DataBlockCreate)

instance JSONSchema DataBlockCreationResponse where
  schema _ =
    Object
      [ typeField "datablock_creation"
      , requiredField "datablock_creation_file_url" nonemptyString
      , requiredField "datablock_creation_timeout" nonemptyString ]

datablockCreationResponseSchema :: Schema
datablockCreationResponseSchema = schema (Proxy :: Proxy DataBlockCreationResponse)

instance JSONSchema User where
  schema _ =
    Object
      [ typeField "user"
      , requiredField "user_username" nonemptyString
      , requiredField "user_name" nonemptyString
      , requiredField "user_email" nonemptyString
      , requiredField "user_datablocks" (unboundedArray True nonemptyString) ]

userSchema :: Schema
userSchema = schema (Proxy :: Proxy User)

instance JSONSchema Job where
  schema _ =
    Object
      [ typeField "job"
      , requiredField "job_id" nonemptyString
      , requiredField "job_template_id" nonemptyString
      , optionalField "job_name" nonemptyString
      , requiredField "job_owner_username" nonemptyString
      , requiredField "job_status" jobStatusSchema
      , requiredField "job_start_time" nonemptyString
      , optionalField "job_end_time" nonemptyString
      , requiredField "job_input_datablocks" jobInputDatablockSetSchema
      , requiredField "job_arguments" jobArgumentPairSetSchema ]

-- The following are schemas internal to JobCreate, thereby being shared by
-- both the Job and JobCreate JSONSchema instances.
jobInputDatablockSchema =
  Object
    [ requiredField "input_datablock_id" nonemptyString
    , optionalField "input_datablock_filter" datablockRecordFilterSchema
    , optionalField "input_datablock_key" nonemptyString ]
jobArgumentPairSchema =
  Object
    [ requiredField "argument_name" nonemptyString
    , requiredField "argument_value" jobArgumentSchema ]
jobArgumentPairSetSchema = unboundedArray True jobArgumentPairSchema
jobInputDatablockSetSchema = nonemptyArray True jobInputDatablockSchema

instance JSONSchema JobArgument where
  schema _ =
        Boolean
    <|> unboundedString
    <|> unboundedNumber
    <|> unboundedArray False jobArgumentSchema -- NOTE: Schema recursion. Hopefully this is okay.

jobArgumentSchema :: Schema
jobArgumentSchema = schema (Proxy :: Proxy JobArgument)

instance JSONSchema JobStatus where
  schema _ =
        Object
          [ typeField "job_status"
          , requiredField "job_status_type" (constantString "success")
          , requiredField "job_status_success_output_datablock_id" unboundedString ]
    <|> Object
          [ typeField "job_status"
          , requiredField "job_status_type" (constantString "failed")
          , requiredField "job_status_failure_reason" unboundedString ]
    <|> Object
          [ typeField "job_status"
          , requiredField "job_status_type" (constantString "canceled")
          , requiredField "job_status_canceled_by" unboundedString ]
    <|> Object
          [ typeField "job_status"
          , requiredField "job_status_type" (constantString "running") ]

jobStatusSchema :: Schema
jobStatusSchema = schema (Proxy :: Proxy JobStatus)

instance JSONSchema JobCreate where
  schema _ =
    Object
      [ typeField "job_create"
      , optionalField "job_create_name" nonemptyString
      , requiredField "job_create_template_id" nonemptyString
      , requiredField "job_create_input_datablocks" jobInputDatablockSetSchema
      , requiredField "job_create_arguments" jobArgumentPairSetSchema ]

jobCreateSchema :: Schema
jobCreateSchema = schema (Proxy :: Proxy JobCreate)

instance JSONSchema JobTemplate where
  schema _ =
    Object
      [ typeField "job_template"
      , requiredField "job_template_id"                   nonemptyString
      , requiredField "job_template_name"                 nonemptyString
      , optionalField "job_template_description"          nonemptyString
      , requiredField "job_template_parameters"           (unboundedArray True jobTemplateParameterSchema)
      , optionalField "job_template_input_datablock_keys" (unboundedArray True nonemptyString)
      , optionalField "job_template_parameter_validation" jobParameterValidationSchema ]

instance JSONSchema JobTemplateParameter where
  schema _ =
    Object
      [ typeField "job_template_parameter"
      , requiredField "job_template_parameter_name"        nonemptyString
      , optionalField "job_template_parameter_description" nonemptyString
      , optionalField "job_template_parameter_default"     jobArgumentSchema
      , requiredField "job_template_parameter_type"        jobTemplateParameterTypeSchema ]

jobTemplateParameterSchema :: Schema
jobTemplateParameterSchema = schema (Proxy :: Proxy JobTemplateParameter)

-- NOTE: This schema involves a recursive schema definition.
instance JSONSchema JobParameterValidation where
  schema _ =
        Object
          [ typeField "job_parameter_validation"
          , requiredField "job_parameter_type" (constantString "parameter_name")
          , requiredField "job_parameter_name" nonemptyString ]
    <|> Object
          [ typeField "job_parameter_validation"
          , requiredField "job_parameter_type"     (constantString "operator")
          , requiredField "job_parameter_operator" (constantString "and" <|> constantString "or")
          , requiredField "job_parameter_operands" (Array (LengthBound (Just 2) Nothing) False jobParameterValidationSchema) ]
    <|> Object
          [ typeField "job_parameter_validation"
          , requiredField "job_parameter_type"     (constantString "operator")
          , requiredField "job_parameter_operator" (constantString "not")
          , requiredField "job_parameter_operand"  jobParameterValidationSchema ]

jobParameterValidationSchema :: Schema
jobParameterValidationSchema = schema (Proxy :: Proxy JobParameterValidation)

instance JSONSchema JobTemplateParameterType where
  schema _ =
        Object
          [ requiredField "datablock" nonemptyString ]
    <|> Object
          [ requiredField "regex" nonemptyString ]
    <|> Object
          [ requiredField "vector" jobTemplateParameterTypeSchema
          , optionalField "vector_size" positiveNumber ]
    <|> constantString "bool"
    <|> constantString "date_time"
    <|> constantString "int"
    <|> constantString "real"
    <|> constantString "string"

jobTemplateParameterTypeSchema :: Schema
jobTemplateParameterTypeSchema = schema (Proxy :: Proxy JobTemplateParameterType)

instance forall a. JSONSchema a => JSONSchema (FilterQuery a) where
  schema p =
        Object
          [ typeField "filter_operator"
          , requiredField "operator" (constantString "and" <|> constantString "or")
          , requiredField "operands" (Array (LengthBound (Just 2) Nothing) False (schema p)) ]
    <|> Object
          [ typeField "filter_operator"
          , requiredField "operator" (constantString "and" <|> constantString "or")
          , requiredField "operand"  (schema p) ]
    <|> Object
          [ typeField "filter_atom"
          , requiredField "filter_atom_name"  (schema (Proxy :: Proxy a))
          , requiredField "filter_atom_match" filterQueryMatchSchema ]

filterQuerySchema :: forall a. JSONSchema a => Proxy a -> Schema
filterQuerySchema _ = schema (Proxy :: Proxy (FilterQuery a))

instance JSONSchema FilterQueryMatch where
  schema _ =
        Object
          [ requiredField "filter_atom_match_name" (foldl (<|>) (constantString "eq") (map constantString ["neq","lt","lte","gt","gte"]))
          , requiredField "filter_atom_match_value" unboundedNumber ]
    <|> Object
          [ requiredField "filter_atom_match_name" (constantString "string" <|> constantString "regex")
          , requiredField "filter_atom_match_value" nonemptyString ]
    <|> Object
          [ requiredField "filter_atom_match_name" (constantString "bool")
          , requiredField "filter_atom_match_value" Boolean ]

filterQueryMatchSchema :: Schema
filterQueryMatchSchema = schema (Proxy :: Proxy FilterQueryMatch)

instance JSONSchema DataBlockSetAtom where
  schema _ = enumString ["datablock_id", "datablock_name_type", "datablock_name", "datablock_contains_field", "datablock_owner_username", "datablock_owner_name", "datablock_owner_email", "datablock_owned"]

datablockSetAtomSchema :: Schema
datablockSetAtomSchema = schema (Proxy :: Proxy DataBlockSetAtom)

instance JSONSchema JobSetAtom where
  schema _ = enumString ["job_id", "job_name", "job_owner_username", "job_owner_name", "job_owner_email", "job_template_id", "job_template_name", "job_status", "job_start_time", "job_end_time", "job_duration", "job_input_datablock_id", "job_output_datablock_id"]

jobSetAtomSchema :: Schema
jobSetAtomSchema = schema (Proxy :: Proxy JobSetAtom)

-- instance JSONSchema DataBlockRecordAtom where
--   schema _ = nonemptyString -- DataBlock Field Name

datablockAtomSchema :: Schema
datablockAtomSchema = schema (Proxy :: Proxy DataBlockRecordAtom)

instance JSONSchema DataBlockRecordAtom where
  schema _ = nonemptyString

datablockRecordFilterSchema :: Schema
datablockRecordFilterSchema = schema (Proxy :: Proxy DataBlockRecordFilter)

instance JSONSchema DataPipeline where
  schema _ =
    Object
      [ typeField "data_pipeline"
      , requiredField "data_pipeline_id"   nonemptyString
      , requiredField "data_pipeline_name" nonemptyString ]

dataPipelineSchema :: Schema
dataPipelineSchema = schema (Proxy :: Proxy DataPipeline)
