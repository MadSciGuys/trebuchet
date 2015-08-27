{-# LANGUAGE OverloadedStrings #-}
module Treb.Test.Gen () where

import Treb.ExtTypes
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe
import Data.Scientific
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.ISO8601
import Codec.MIME.Type
import ProtoDB.Types

import Numeric (showHex)
import Data.List (nubBy)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Word (Word64)

import qualified Codec.MIME.Type as MIME
import qualified Data.Vector as V
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Treb.Test.Samples as Samples
import qualified Data.Set as S
import qualified Treb.Test.Samples as Samples

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen g = oneof [ pure Nothing, fmap Just g ]

boolGen :: Gen Bool
boolGen = elements [True, False]

numberGen :: Gen Scientific
numberGen = fromFloatDigits <$> floatGen

floatGen :: Gen Float
floatGen = choose (1.0, 10000.0)

stringGen :: Gen Text
stringGen = elements ["A", "B", "C", "D", "E"]

vectorGen :: Arbitrary a => Gen (Vector a)
vectorGen = V.fromList <$> listOf arbitrary

regexGen :: Gen Text
regexGen = elements [".*", "(123|abc)*$"]

instance Arbitrary DataBlock where
  arbitrary = do
    dbId <- hexGen
    field0 <- arbitrary
    fields <- (nubBy (\x y -> datablockFieldName x == datablockFieldName y) . (field0:)) <$> arbitrary
    records <- oneof [ pure Nothing, Just <$> recordSetGen dbId fields ]
    DataBlock <$> pure dbId
              <*> arbitrary
              <*> pure fields
              <*> (maybe (choose (1, 100000)) (pure . V.length) records)
              <*> arbitrary
              <*> pure records

usernameGen :: Gen Text
usernameGen = elements [ "mswan", "twhitak", "afermier", "tek" ]

recordSetGen :: DataBlockId -> [DataBlockField] -> Gen (V.Vector DataBlockRecord)
recordSetGen dbId fs = sized $ \n -> do
  recordCount <- choose (1, n)
  V.generateM recordCount (flip variant (recordGen dbId fs))

recordGen :: DataBlockId -> [DataBlockField] -> Gen DataBlockRecord
recordGen dbId fs = V.mapM (cellGen dbId) (V.fromList fs)

cellGen :: DataBlockId -> DataBlockField -> Gen DataBlockCell
cellGen dbId f =
  case datablockFieldType f of
    DBInt ->
      numberGen
    DBReal ->
      numberGen
    DBString ->
      stringGen
    DBBinary _ ->
      binaryGen dbId (datablockFieldName f)
    DBVector n ty ->
      DataBlockCellVector <$>
        V.generateM n (flip variant (cellGen dbId (f { datablockFieldType = ty })))
    DBDateTime ->
      DataBlockCellString <$> pack <$> formatISO8601 <$> (UTCTime <$> (ModifiedJulianDay <$> arbitrary) <*> (pure (secondsToDiffTime 0)))
    where
      numberGen = DataBlockCellNumber <$> fromFloatDigits <$> (choose (1, 100000) :: Gen Float)
      stringGen = DataBlockCellString <$> elements ["A", "B", "C", "D", "E"]
      binaryGen dbId dbFieldName = DataBlockCellString <$> fmap (("https://pku-api.pdms.jnj.com/datablock_binary/" <> dbId <> "/" <> dbFieldName <> "/") <>) (pack <$> show <$> (choose (1,100000) :: Gen Int))

instance Arbitrary DataBlockCell where
  arbitrary =
    oneof 
      [ DataBlockCellBool   <$> boolGen
      , DataBlockCellNumber <$> numberGen
      , DataBlockCellString <$> stringGen
      , DataBlockCellVector <$> vectorGen ]

instance Arbitrary DataBlockCreate where
  arbitrary = do
    dbId <- hexGen
    fields <- maybeGen datablockFieldSetGen
    records <- promote (recordSetGen dbId <$> fields)
    DataBlockCreate <$> datablockNameGen
                    <*> pure fields
                    <*> pure records

instance Arbitrary DataBlockField where
  arbitrary =
    DataBlockField <$> elements Samples.datablockFieldNames
                   <*> arbitrary
                   <*> arbitrary

instance Arbitrary DataBlockFieldType where
  arbitrary =
    oneof
      [ pure DBInt
      , pure DBReal
      , pure DBString
      , DBBinary <$> maybeGen (elements Samples.mimeTypes)
      , DBVector <$> choose (0, 5) <*> arbitrary ]

-- TODO: Make this not terrible.
datablockFieldSetGen :: Gen [DataBlockField]
datablockFieldSetGen = (:[]) <$> arbitrary

-- names :: V.Vector Text
-- names = V.fromList Samples.datablockFieldNames
-- 
-- datablockFieldSetGen :: Gen [DataBlockField]
-- datablockFieldSetGen = S.toList <$> datablockFieldSetGen
-- 
-- datablockFieldSetGen' :: Set DataBlockField -> Gen (Set DataBlockField)
-- datablockFieldSetGen' inputSet = do
--   set 

-- sized $ \n -> do
--   f <- oneof [ pure id, fmap (const . datablockFieldName) datablockFieldSetGen ]
--   len' <- choose (1, n)
--   let len = min len' $ V.length names 
--   let nameSet = S.fromList [1..len]
--   until ((== 0) . size . fst) $ \ (s, g) ->
--     let k = fromMaybe 1 (S.lookupLT i s <|> S.lookupGE i s)
--     in ( S.delete k s,
--          g *> DataBlockField <$> pure (names !! k)
--                              <*> datablockFieldTypeGen
--                              <*> arbitrary )
instance Arbitrary DataBlockName where
  arbitrary = oneof [ adhoc, recipe, jobResult, alias ]
    where
      adhoc = do
        dbName <- datablockNameGen
        return $ AdHocName dbName
      recipe = resize 5 $ do
        product <- elements Samples.productNames
        ingredients <- listOf1 ingredientNameGen
        let compound = Text.intercalate " " (product:ingredients)
        recipes <- listOf1 $ elements Samples.recipeNames
        return $ RecipeName compound $ List.nub recipes
      jobResult = do
        id <- Text.pack <$> show <$> (choose (10000, 99999) :: Gen Int)
        model <- elements Samples.modelNames
        srcDbName <- datablockNameGen
        let name = Text.intercalate " " [model, srcDbName]
        return $ JobResultName id name
      alias = do
        adjective <- elements Samples.adjectives
        return $ AliasName $ adjective <> " Data Plot"

-- Helpers
datablockNameGen :: Gen Text.Text
datablockNameGen = do
  adjective <- elements Samples.adjectives
  product   <- elements Samples.productNames
  datablock <- elements Samples.datablockNames
  return $ Text.intercalate " " [adjective, product, datablock]

ingredientNameGen :: Gen Text.Text
ingredientNameGen = oneof [ jnjIngredient, elements Samples.elementNames ]
  where
    jnjIngredient :: Gen Text.Text
    jnjIngredient = choose (100000 :: Int,999999) >>= return . ("JNJ-" <>) . Text.pack . show

instance Arbitrary DataBlockNameType where
  arbitrary = elements [AdHocType, RecipeType, JobResultType, AliasType]

instance Arbitrary DataBlockSource where
  arbitrary =
    oneof 
      [ APIUserSource <$> usernameGen
      , APIJobSource <$> idGen
      , DataPipelineSource <$> idGen ]

idGen :: Gen Text
idGen = pack <$> show <$> (choose (100,10000) :: Gen Int)

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = sized $ \n -> choose (0,n) >>= flip V.generateM (flip variant arbitrary)

instance Arbitrary DataBlockCreationResponse where
  arbitrary =
    DataBlockCreationResponse <$> urlGen
                              <*> arbitrary

-- TODO: Improve.
urlGen = elements [ "http://www.google.com/" ]

instance Arbitrary UTCTime where
  arbitrary =
    UTCTime <$> (ModifiedJulianDay <$> (+ 57260) <$> arbitrary)
            <*> (secondsToDiffTime <$> choose (0, 86400))

instance Arbitrary DataPipeline where
  arbitrary =
    DataPipeline <$> hexGen
                 <*> elements ["Empower", "OSI Pi", "EDW" ]

hexGen :: Gen Text
hexGen = pack <$> ($ "") <$> showHex <$> (choose (2^31, 2^32) :: Gen Int)

instance Arbitrary User where
  arbitrary = do
    (username, name) <- elements
      [ ("mswan", "Micheal Swan")
      , ("twhitak", "Travis Whitaker")
      , ("afermier", "Adam Fermier") ]
    User <$> pure username
         <*> pure name
         <*> pure (username <> "@its.jnj.com")
         <*> listOf hexGen

instance Arbitrary Job where
  arbitrary = do
    status <- arbitrary
    startTime <- arbitrary
    endOffset <- choose (1, 1000)
    endTime <- pure $ case status of
      JobRunning -> Nothing
      _ -> Just $ addUTCTime (fromInteger endOffset) startTime
    Job <$> hexGen
        <*> arbitrary
        <*> usernameGen
        <*> pure status
        <*> pure startTime
        <*> pure endTime

instance Arbitrary JobCreate where
  arbitrary = do
    JobCreate <$> maybeGen stringGen
              <*> hexGen
              <*> listOf ((,,) <$> hexGen <*> maybeGen stringGen <*> arbitrary)
              <*> listOf ((,) <$> elements Samples.datablockFieldNames <*> arbitrary)

instance Arbitrary JobArgument where
  arbitrary = oneof [ JobArgumentBool   <$> arbitrary
                    , JobArgumentNumber <$> arbitrary
                    , JobArgumentString <$> stringGen
                    , JobArgumentVector <$> arbitrary ]

instance Arbitrary a => Arbitrary (FilterQuery a) where
  arbitrary = oneof [ FilterQueryAnd  <$> arbitrary
                    , FilterQueryOr   <$> arbitrary
                    , FilterQueryNot  <$> arbitrary
                    , FilterQueryAtom <$> arbitrary <*> arbitrary ]

instance Arbitrary DataBlockRecordAtom where
  arbitrary = DataBlockRecordAtom <$> stringGen

instance Arbitrary Scientific where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary FilterQueryMatch where
  arbitrary = oneof
    [ FilterQueryMatchEq     <$> arbitrary
    , FilterQueryMatchNeq    <$> arbitrary
    , FilterQueryMatchLt     <$> arbitrary
    , FilterQueryMatchLte    <$> arbitrary
    , FilterQueryMatchGt     <$> arbitrary
    , FilterQueryMatchGte    <$> arbitrary
    , FilterQueryMatchBool   <$> arbitrary
    , FilterQueryMatchString <$> stringGen
    , FilterQueryMatchRegex  <$> regexGen ]

instance Arbitrary JobStatus where
  arbitrary = oneof
    [ JobSuccess  <$> hexGen
    , JobFailed   <$> pure "blah blah blah, reasoning and such."
    , JobCanceled <$> usernameGen
    , pure JobRunning ]

instance Arbitrary JobTemplate where
  arbitrary =
    JobTemplate <$> hexGen
                <*> stringGen
                <*> maybeGen stringGen
                <*> arbitrary
                <*> maybeGen (listOf stringGen)
                <*> arbitrary

instance Arbitrary JobParameterValidation where
  arbitrary = oneof
    [ JobParameterValidationName <$> elements Samples.datablockFieldNames
    , JobParameterValidationAnd  <$> arbitrary
    , JobParameterValidationOr   <$> arbitrary
    , JobParameterValidationNot  <$> arbitrary ]

instance Arbitrary JobTemplateParameter where
  arbitrary =
    JobTemplateParameter <$> stringGen
                         <*> maybeGen (pure "This is a description...")
                         <*> arbitrary
                         <*> arbitrary

instance Arbitrary JobTemplateParameterType where
  arbitrary = oneof
    [ pure JobTemplateParameterTypeBool
    , pure JobTemplateParameterTypeDateTime
    , pure JobTemplateParameterTypeInt
    , pure JobTemplateParameterTypeReal
    , pure JobTemplateParameterTypeString
    , pure JobTemplateParameterTypeDataBlockName
    , pure JobTemplateParameterTypeDataBlockFieldName
    , pure JobTemplateParameterTypeDataBlockKey
    , JobTemplateParameterTypeEnum   <$> listOf1 stringGen
    , JobTemplateParameterTypeRegex  <$> regexGen
    , JobTemplateParameterTypeVector <$> maybeGen (choose (2,5)) <*> arbitrary ]

instance Arbitrary DataBlockSetAtom where
  arbitrary = elements
    [ DataBlockId
    , DataBlockNameType
    , DataBlockName
    , DataBlockContainsField
    , DataBlockOwned
    , DataBlockOwnerUsername
    , DataBlockOwnerName
    , DataBlockOwnerEmail ]

instance Arbitrary JobSetAtom where
  arbitrary = elements
    [ JobId
    , JobName
    , JobOwnerUsername
    , JobOwnerName
    , JobOwnerEmail
    , JobTemplateId
    , JobTemplateName
    , JobStatus
    , JobStartTime
    , JobEndTime
    , JobDuration
    , JobInputDataBlockId
    , JobOutputDataBlockId ]

instance Arbitrary CASAuth where
  arbitrary = (CASAuth . ("ST-" <>)) <$> hexGen
