{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Treb.Test.JSON (tests, toJsonTests, fromJsonTests) where

-- General
import qualified Data.Aeson as Aeson (Result(..))
import Data.Aeson hiding (Result)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text(..), intercalate, pack, unpack)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, decodeASCII)
import Data.Text.Manipulate (prependLines)
import Data.Monoid ((<>))
import Data.Maybe
import Data.List (nub)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.ByteString.Lazy (ByteString)
import Data.Bool (bool)
import Control.Applicative
import Data.Functor

-- Testing
import Test.Tasty
import Test.Tasty.TH (testGroupGenerator)
import Test.Tasty.QuickCheck hiding (Result(..), reason)
-- hiding (Result(..), property, succeeded, failed)
import Test.Tasty.Ingredients.Basic (Quiet(..), ListTests(..))
import Data.JSON.Schema (Schema)
import Data.JSON.Schema.Validate
import Treb.Test.Samples
import Test.QuickCheck.Property (succeeded, failed, verbose)
import qualified Test.QuickCheck.Property as QC (Result(..))

import Data.JSON.Schema
import Treb.Test.Schema

-- Trebuchet
import Treb.ExtTypes
import Treb.ExtJSON

-- Trebuchet Testing
import Treb.Test.Gen
import Treb.Test.Utils
import qualified Treb.Test.Schema as Schema

tests :: TestTree
tests =
  testGroup "JSON Validation"
    [ toJsonTests, fromJsonTests ]

data ToJsonTest a = forall t. Testable t => ToJsonTest String (a -> t)
data FromJsonTest a = forall t. Testable t => FromJsonTest String (a -> t)

toJsonTests :: TestTree
toJsonTests =
  testGroup "JSON Encode"
    [ runTest datablockName
    , runTest datablockField
    , runTest datablockSource
    , runTest datablockCell
    , runTest datablock ]
  where
    -- Helpers
    runTest :: (Arbitrary a, Show a) => ToJsonTest a -> TestTree
    runTest (ToJsonTest name test) = testProperty name test

    toJsonTest :: forall a. (Arbitrary a, Eq a, ToJSON a, JSONSchema a) => Text -> ToJsonTest a
    toJsonTest name = ToJsonTest (unpack name) (toJsonValidate name (schema (Proxy :: Proxy a)))

    -- Test Definitions
    datablockName :: ToJsonTest DataBlockName
    datablockName = toJsonTest "DataBlockName"

    datablockField :: ToJsonTest DataBlockField
    datablockField = toJsonTest "DataBlockField"

    datablockSource :: ToJsonTest DataBlockSource
    datablockSource = toJsonTest "DataBlockSource"

    datablockCell :: ToJsonTest DataBlockCell
    datablockCell = toJsonTest "DataBlockCell"

    datablock :: ToJsonTest DataBlock
    datablock = toJsonTest "DataBlock"

fromJsonTests :: TestTree
fromJsonTests =
  testGroup "JSON Decode"
    [ runTest datablockName
    , runTest datablockField
    , runTest datablockSource
    , runTest datablockCell
    , runTest datablock ]
  where
    -- Helpers
    runTest :: (Arbitrary a, Show a) => FromJsonTest a -> TestTree
    runTest (FromJsonTest name test) = testProperty name test

    fromJsonTest :: forall a. (Arbitrary a, Show a, Eq a, ToJSON a, FromJSON a, JSONSchema a) => Text -> FromJsonTest a
    fromJsonTest name = FromJsonTest (unpack name) (fromJsonValidate name (schema (Proxy :: Proxy a)))

    -- Test Definitions
    datablockName :: FromJsonTest DataBlockName
    datablockName = fromJsonTest "DataBlockName"

    datablockField :: FromJsonTest DataBlockField
    datablockField = fromJsonTest "DataBlockField"

    datablockSource :: FromJsonTest DataBlockSource
    datablockSource = fromJsonTest "DataBlockSource"

    datablockCell :: FromJsonTest DataBlockCell
    datablockCell = fromJsonTest "DataBlockCell"

    datablock :: FromJsonTest DataBlock
    datablock = fromJsonTest "DataBlock"

-- QuickCheck Properties Builders
fromJsonValidate :: forall a. (Arbitrary a, Eq a, ToJSON a, FromJSON a, Show a) => Text -> Schema -> a -> QC.Result
fromJsonValidate typeName _ obj =
  bool failure success objMatch
  where
    objMatch :: Bool
    objMatch = maybe False (== obj) decodedObj

    success :: QC.Result
    success = succeeded

    failure :: QC.Result
    failure = failed { QC.reason = unpack failureReason }

    encodedObj :: ByteString
    encodedObj = encode obj

    decodedObj :: Maybe a
    decodedObj = decode encodedObj

    autoTypeSig :: Text
    autoTypeSig = typeName <> " -> " <> typeName

    decodeResult :: Aeson.Result a
    decodeResult = fromJSON $ toJSON obj

    failureDescription :: Text
    failureDescription = 
      "ToJSON "<> typeName <> " and FromJSON " <> typeName <> " instances do not match.\n" <>
      "'decode . encode :: " <> autoTypeSig <> "' should be extensionally equal to 'id :: " <> autoTypeSig <> "'.\n"

    encodedJson :: Text
    encodedJson =
      "Encoded JSON:\n" <>
      (indentStr $ prettyJSON $ toJSON obj) <> "\n"

    decodedHaskellValue :: Text
    decodedHaskellValue =
      maybe "Failed to decode JSON."
        ("Decoded Haskell Value:\n" <>) 
        (indentStr <$> pack <$> show <$> decodedObj) <> "\n"

    decodeError :: Text
    decodeError =
      maybe ""
        ("Decode Error: " <>)
        (case decodeResult of
          Aeson.Success _ -> Nothing
          Aeson.Error err -> Just $ pack err) <> "\n"

    failureReason :: Text
    failureReason =
      failureDescription <> "\n" <>
      decodedHaskellValue <> "\n" <>
      decodeError <> "\n" <>
      encodedJson

toJsonValidate :: forall a. (Arbitrary a, Eq a, ToJSON a) => Text -> Schema -> a -> QC.Result
toJsonValidate typeName schema obj =
  bool failure success noErrors
  where
    noErrors :: Bool
    noErrors = V.null objErrors

    success :: QC.Result
    success = succeeded

    failure :: QC.Result
    failure = failed { QC.reason = unpack failureReason }

    objJson :: Value
    objJson = toJSON obj

    objErrors :: Vector ValidationError
    objErrors = validate schema objJson
    
    validationErrors :: Text
    validationErrors = fromMaybe "None" $ showValidationErrors objErrors

    failureReason :: Text
    failureReason =
      -- Description
      "ToJSON " <> typeName <> " instance did not meet specification.\n\n" <>
      -- JSON payload
      "Generated JSON:\n" <>
      indentStr (prettyJSON objJson) <>
      "\n\n" <>
      -- Validation errors
      "Validation Errors:\n" <>
      indentStr validationErrors

indentStr :: Text -> Text
indentStr = (<>) "    " . T.concatMap (\c -> if c == '\n' then "\n    " else T.singleton c)

nestStr :: Text -> Text
nestStr = T.concatMap (\c -> if c == '\n' then "\n| " else T.singleton c)

prettyJSON :: Value -> Text
prettyJSON = toStrict . decodeUtf8 . encodePretty

showValidationErrors :: Foldable t => t ValidationError -> Maybe Text
showValidationErrors = filterMaybe (not . T.null) . foldMap showValidationError

showValidationError :: ValidationError -> Text
showValidationError (ValidationError path errorType) =
  "\n" <> case errorType of
    ChoiceError cs value ->
      "Choice" <> (nestStr $ V.foldl (\r (i,index) -> r <> "\n" <> "[" <> (pack $ show index) <> "]" <> (V.foldl (\s j -> s <> showValidationError j <> "") "" i <> "")) "" $ V.zip cs $ V.fromListN (length cs) [1..])
    Mismatch schema value ->
      "Mismatch" <> (nestStr ("\nSchema" <> (nestStr ("\n" <> (pack $ show $ schema))) <> "\n" <> "Value" <> (nestStr ("\n" <> prettyJSON value))))
    BoundError bound num ->
      "Bound"
    LengthBoundError lengthBound int ->
      "Length Bound"
    TupleLength intA intB ->
      "Tuple Length"
    MissingRequiredField text ->
      "Field Missing" <> nestStr ("\n" <> text)
    NonUniqueArray valToIntMap ->
      "Non-Unique Array Error"
