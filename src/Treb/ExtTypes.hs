-- | Contained is a set of types that serve as the units of exchange between
-- the client and server. Most contained types will have ToJSON/FromJSON
-- instances. These types are formed to accommodate The implementation of
-- those instances. Any value that does not belong in the API's JSON messages
-- does not belong here.
--
-- The following is a list of some desired properties:
--   * (fromJSON . toJSON) should be equal to the Aeson Success constructor.
--   * 
--   * ID's are always expected to be strings.
module Treb.ExtTypes where

import Data.Text.Read

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Scientific (Scientific)

import qualified Codec.MIME.Type as MIME
import qualified Treb.Types as IT

-- DataBlock
data DataBlock = DataBlock
        { datablockId            :: DataBlockId
        , datablockName          :: DataBlockName
        , datablockFields        :: [DataBlockField]
        , datablockRecordCount   :: Int
        -- | datablockSource = Nothing for datablocks with an unknown source.
        , datablockSource        :: Maybe DataBlockSource
        -- | datablockRecords = Nothing in the case of a metadata response.
        , datablockRecords       :: Maybe (Vector DataBlockRecord) }
        deriving (Eq, Show)

data DataBlockField = DataBlockField
        { datablockFieldName    :: Text
        , datablockFieldType    :: DataBlockFieldType
        , datablockFieldIndexed :: Maybe Bool }
        deriving (Eq, Show)

data DataBlockFieldType = DBInt
                        | DBReal
                        | DBString
                        -- | DBBinary contains Nothing if the MIME type is unknown.
                        | DBBinary (Maybe MIME.Type)
                        | DBVector Int DataBlockFieldType
                        | DBDateTime
        deriving (Eq, Show)

type DataBlockRecord = Vector DataBlockCell

-- | While a DataBlock cell has six types presently, there are only three JSON
-- types being used to represent them. Both real and integer values are
-- represented with a DataBlockCellNumber. String, binary, and date-time values
-- are represented with a DataBlockCellString. In the case of binary values, a
-- URL to the corresponding data is provided. In the case of date-time, is a
-- string in the ISO 8601 UTC format.
data DataBlockCell = DataBlockCellBool   Bool
                   | DataBlockCellNumber Scientific
                   | DataBlockCellString Text
                   | DataBlockCellVector (Vector DataBlockCell)
        deriving (Eq, Show)

data DataBlockSource = APIUserSource
                        { apiUserSourceUsername :: Text }
                     | APIJobSource
                        { apiJobSourceJobId :: JobId }
                     | DataPipelineSource
                        { dataPipelineSourceId :: DataPipelineId }
        deriving (Eq, Show)

data DataBlockCreate = DataBlockCreate
        { datablockCreateName    :: Text
        , datablockCreateFields  :: Maybe [DataBlockField]
        , datablockCreateRecords :: Maybe (Vector DataBlockRecord) }
        deriving (Eq, Show)

data DataBlockCreationResponse = DataBlockCreationResponse
        { datablockCreationResponseFileURL :: Text
        , datablockCreationResponseTimeout :: UTCTime }
        deriving (Eq, Show)

type DataBlockId = Int

-- | A reference to a datablock. Each datablock has at least one unique element
--   of this type, elsewhere known as the "canonical name". 'AdHocName',
--   'RecipeName', and 'JobResultName' values are considered canonical names,
--   while 'AliasName' values are not. In other words, each datablock has a
--   single unique name of either the 'AdHocName', 'RecipeName', or
--   'JobResultName' types, with zero or more additional names of the
--   'AliasName' type.
data DataBlockName = -- | Ad-hoc (i.e. user provided) datablock.
                     AdHocName Text
                     -- | Recipe datablock, from an automated data pipeline.
                   | RecipeName {
                       compoundName :: Text   -- ^ Compound name.
                     , recipeNames  :: [Text] -- ^ Recipe names in topological
                                                --   order.
                     }
                     -- | Job execution result, including the 'Job' ID and name.
                     -- Note that the job ID is in textual form here. This is
                     -- intended to keep the API interface agnostic of that
                     -- implementation decision.
                   | JobResultName Text Text
                     -- | A datablock alias.
                   | AliasName Text
        deriving (Eq, Ord, Show)

data DataBlockNameType = -- | Ad-hoc (i.e. user provided) datablock.
                         AdHocType
                         -- | Recipe datablock, from an automated data pipeline.
                       | RecipeType
                         -- | Job execution result.
                       | JobResultType
                         -- | A datablock alias.
                       | AliasType
                       deriving (Eq, Ord, Show)

-- User
data User = User
        { userUsername   :: Text
        , userName       :: Text
        , userEmail      :: Text
        , userDataBlocks :: [DataBlockId] }
        deriving (Eq, Show)

-- Job
data Job = Job
        { jobId              :: JobId
        , jobCreate          :: JobCreate
        , jobOwnerUsername   :: Text
        , jobStatus          :: JobStatus
        , jobStartTime       :: UTCTime
        , jobEndTime         :: Maybe UTCTime }
        deriving (Eq, Show)

jobOutputDataBlockId :: Job -> Maybe DataBlockId
jobOutputDataBlockId j =
  case jobStatus j of
    JobSuccess so -> Just so
    _             -> Nothing


data JobStatus = JobSuccess { jobSuccessOutputDataBlockId :: DataBlockId }
               | JobFailed { jobFailedReason :: Text }
               | JobCanceled { jobCanceledUsername :: Text }
               | JobRunning
        deriving (Eq, Show)

type JobId = Int

data JobCreate = JobCreate
        { jobCreateName            :: Maybe Text
        , jobCreateTemplateId      :: JobTemplateId
        , jobCreateInputDataBlocks :: [(DataBlockId, Maybe Text, Maybe DataBlockRecordFilter)] -- 2nd element is the DataBlock key (a.k.a. the "tag")
        , jobCreateArguments       :: [(Text, JobArgument)] }
        deriving (Eq, Show)

data JobTemplate = JobTemplate
        { jobTemplateId                  :: JobTemplateId
        , jobTemplateName                :: Text
        , jobTemplateDescription         :: Maybe Text
        , jobTemplateParameters          :: [JobTemplateParameter]
        , jobTemplateInputDatablockKeys  :: Maybe [Text]
        , jobTemplateParameterValidation :: Maybe JobParameterValidation }
        deriving (Eq, Show)

type JobTemplateId = Int

data JobTemplateParameter = JobTemplateParameter
        { jobTemplateParameterDisplayName :: Text
        , jobTemplateParameterKeyName     :: Text
        , jobTemplateParameterDescription :: Maybe Text
        , jobTemplateParameterDefault     :: Maybe JobArgument
        , jobTemplateParameterType        :: JobTemplateParameterType }
        deriving (Eq, Show)

data JobTemplateParameterType = JobTemplateParameterTypeBool
                              | JobTemplateParameterTypeDateTime
                              | JobTemplateParameterTypeInt
                              | JobTemplateParameterTypeReal
                              | JobTemplateParameterTypeString
                              -- The set of accepted values corresponds with the clients contextual understanding of "the current subset of datablocks"
                              | JobTemplateParameterTypeDataBlockName
                              -- The set of accepted values corresponds with the clients contextual understanding of "the current datablock"
                              | JobTemplateParameterTypeDataBlockFieldName
                              -- The set of accepted values corresponds with the current job template's input datablock keys set.
                              | JobTemplateParameterTypeDataBlockKey
                              | JobTemplateParameterTypeEnum      [Text]
                              | JobTemplateParameterTypeRegex     Text
                              | JobTemplateParameterTypeVector    (Maybe Int) JobTemplateParameterType
        deriving (Eq, Show)

data JobParameterValidation = JobParameterValidationName Text
                            | JobParameterValidationAnd  [JobParameterValidation]
                            | JobParameterValidationOr   [JobParameterValidation]
                            | JobParameterValidationNot  JobParameterValidation
        deriving (Eq, Show)

data JobArgument = JobArgumentBool   Bool
                 | JobArgumentNumber Scientific
                 | JobArgumentString Text
                 | JobArgumentVector (Vector JobArgument)
        deriving (Eq, Show)

-- Filtering
data FilterQuery a = FilterQueryAnd  [FilterQuery a]
                   | FilterQueryOr   [FilterQuery a]
                   | FilterQueryNot  (FilterQuery a)
                   | FilterQueryAtom FilterQueryMatch a
        deriving (Eq, Show)

data FilterQueryMatch = -- Binary numeric matchers
                        FilterQueryMatchEq  Scientific
                      | FilterQueryMatchNeq Scientific
                      | FilterQueryMatchLt  Scientific
                      | FilterQueryMatchLte Scientific
                      | FilterQueryMatchGt  Scientific
                      | FilterQueryMatchGte Scientific
                        -- Others
                      | FilterQueryMatchBool   Bool
                      | FilterQueryMatchString Text
                      | FilterQueryMatchRegex  Text
        deriving (Eq, Show)

data DataBlockSetAtom = DataBlockId
                      | DataBlockNameType
                      | DataBlockName
                      | DataBlockContainsField
                      | DataBlockOwned
                      | DataBlockOwnerUsername
                      | DataBlockOwnerName
                      | DataBlockOwnerEmail
        deriving (Eq, Show)

data JobSetAtom = JobId
                | JobName
                | JobOwnerUsername
                | JobOwnerName
                | JobOwnerEmail
                | JobTemplateId
                | JobTemplateName
                | JobStatus
                | JobStartTime
                | JobEndTime
                | JobDuration
                | JobInputDataBlockId -- A match occurs if any of the job's input DataBlock ID's match.
                | JobOutputDataBlockId
        deriving (Eq, Show)

newtype DataBlockRecordAtom = DataBlockRecordAtom Text
        deriving (Eq, Show)

type DataBlockSetFilter    = FilterQuery DataBlockSetAtom
type JobSetFilter          = FilterQuery JobSetAtom
type DataBlockRecordFilter = FilterQuery DataBlockRecordAtom

-- Data Pipeline
data DataPipeline = DataPipeline
        { dataPipelineId :: DataPipelineId
        , dataPipelineName :: Text }
        deriving (Eq, Show)

type DataPipelineId = Text
