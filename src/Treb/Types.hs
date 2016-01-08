{-|
Module:      Treb.Types
Description: Trebuchet types.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX

The types in this module represent both server-internal representations of the
objects of exchange (datablocks, queries, jobs, users, etc.) and discrete
messages sent between the server and API callers.
-}

{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , TypeSynonymInstances
           , StandaloneDeriving
           , LambdaCase
           #-}

module Treb.Types where

import qualified Codec.MIME.Type as MIME
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Data.Maybe (isJust, fromMaybe)
import Data.Time
import Data.Word
import Foreign.Ptr
import ProtoDB.Types
import Treb.BadRegex
import Treb.Filter

-- | A reference to a datablock. Each datablock has at least one unique element
--   of this type, elsewhere known as the "canonical name". 'AdHocName',
--   'RecipeName', and 'JobResultName' values are considered canonical names,
--   while 'AliasName' values are not. In other words, each datablock has a
--   single unique name of either the 'AdHocName', 'RecipeName', or
--   'JobResultName' types, with zero or more additional names of the
--   'AliasName' type.
data DataBlockName = -- | Ad-hoc (i.e. user provided) datablock.
                     AdHocName {
                       shortName    :: T.Text -- ^ User provided name.
                     , uploaderName :: T.Text -- ^ Uploader's username.
                     }
                     -- | Recipe datablock, from an automated data pipeline.
                   | RecipeName {
                       compoundName :: T.Text   -- ^ Compound name.
                     , recipeNames  :: [T.Text] -- ^ Recipe names in topological
                                                --   order.
                     , pipelineName :: T.Text   -- ^ Data "pipeline" name.
                     }
                     -- | Job execution result, including the 'Job' ID and name.
                   | JobResultName Word64 T.Text
                     -- | A datablock alias.
                   | AliasName T.Text
                   deriving (Eq, Ord, Show)

-- | The type of a 'DataBlockName'.
data DataBlockNameType = -- | Ad-hoc (i.e. user provided) datablock.
                         AdHocType
                         -- | Recipe datablock, from an automated data pipeline.
                       | RecipeType
                         -- | Job execution result.
                       | JobResultType
                         -- | A datablock alias.
                       | AliasType
                       deriving (Eq, Ord, Show)

-- | Get the type of a 'DataBlockName'.
dataBlockNameType :: DataBlockName -> DataBlockNameType
dataBlockNameType (AdHocName _ _)       = AdHocType
dataBlockNameType (RecipeName _ _ _)    = RecipeType
dataBlockNameType (JobResultName _ _) = JobResultType
dataBlockNameType (AliasName _)       = AliasType

isAdHocType :: DataBlockName -> Bool
isAdHocType (AdHocName _ _) = True
isAdHocType _             = False

isRecipeType :: DataBlockName -> Bool
isRecipeType (RecipeName _ _ _) = True
isRecipeType _                = False

isJobResultType :: DataBlockName -> Bool
isJobResultType (JobResultName _ _) = True
isJobResultType _                   = False

isAliasType :: DataBlockName -> Bool
isAliasType (AliasName _) = True
isAliasType _             = False

-- | Datablock filter atoms. Each of these constructors represents a predicate
--   on datablocks, allowing API callers to effectively search for datablocks
--   based on their type, name, owners, etc.
data DataBlockFilter = -- | Match datablocks with a name of a specific type.
                       NameType DataBlockNameType
                       -- | Match a datablock name exactly.
                     | NameExact DataBlockName
                       -- | Match all datablock names recognized by the
                       --   provided regular expression.
                     | NameRegex T.Text
                       -- | Match a datablock ID exactly.
                     | IdExact Word64
                       -- | Match datablocks that have an owner.
                     | Owned
                       -- | Match a datablock owner's user ID exactly.
                     | UserIdExact Word64
                       -- | Match a datablock owner's user name exactly.
                     | UserNameExact T.Text
                       -- | Match all datablocks whose owner's user name is
                       --   recognized by the provided regular expression.
                     | UserNameRegex T.Text
                       -- | Match a datablock owner's real name exactly.
                     | RealNameExact T.Text
                       -- | Match all datablocks whose owner's real name is
                       --   recognized by the provided regular expression.
                     | RealNameRegex T.Text
                       -- | Match a datablock owner's email address exactly.
                     | EmailExact T.Text
                       -- | Match all datablocks whose owner's email address
                       --   is recognized by the provided regular
                       --   expression.
                     | EmailRegex T.Text
                       -- | Match all datablocks containing the specified
                       --   field.
                     | ContainsField DataBlockField
                     deriving (Eq, Ord, Show)

deriving instance Eq (Filter (M.Map DataBlockName DataBlock))
deriving instance Ord (Filter (M.Map DataBlockName DataBlock))
deriving instance Show (Filter (M.Map DataBlockName DataBlock))

instance Filterable (M.Map DataBlockName DataBlock) where
    type Atom (M.Map DataBlockName DataBlock) = DataBlockFilter
    appAtom (NameType t) = M.filter ((t ==) . dataBlockNameType . dbName)
    appAtom (NameExact n) = lookupMap n
    appAtom (NameRegex e) = let compMatch (AdHocName n _) = n `badRegexBool` e
                                compMatch (RecipeName cn rns pn) = (pn `badRegexBool` e) || (cn `badRegexBool` e) || all (`badRegexBool` e) rns
                                compMatch (JobResultName i n) = (T.pack (show i) `badRegexBool` e) || (n `badRegexBool` e)
                                compMatch (AliasName n) = n `badRegexBool` e
                            in M.filter (compMatch . dbName)
    appAtom (IdExact i) = M.filter ((i ==) . dbID)
    appAtom Owned = M.filter (isJust . dbOwner)
    appAtom (UserIdExact i) = M.filter (fromMaybe False . (((i ==) . userID) <$>) . dbOwner)
    appAtom (UserNameExact n) = M.filter (fromMaybe False . (((n ==) . userName) <$>) . dbOwner)
    appAtom (UserNameRegex e) = M.filter (fromMaybe False . (((`badRegexBool` e) . userName) <$>) . dbOwner)
    appAtom (RealNameExact n) = M.filter (fromMaybe False . (((n ==) . realName) <$>) . dbOwner)
    appAtom (RealNameRegex e) = M.filter (fromMaybe False . (((`badRegexBool` e) . realName) <$>) . dbOwner)
    appAtom (EmailExact a) = M.filter (fromMaybe False . (((a ==) . email) <$>) . dbOwner)
    appAtom (EmailRegex e) = M.filter (fromMaybe False . (((`badRegexBool` e) . email) <$>) . dbOwner)
    appAtom (ContainsField f) = M.filter (elem f . dbFields)
    neg = flip M.difference
    conj = M.intersection
    disj = M.union

-- | Name, type, and array description of a datablock field. Datablock field
--   names are expected to be unique within a datablock.
data DataBlockField = DataBlockField {
    fieldName    :: T.Text
  , fieldType    :: ProtoCellType
  -- | If the field is a scalar, this will equal the empty list. If it is a
  --   vector, it will contain a list of integers describing the maximum length
  --   in each dimension in row-major order. For example, a three dimensional
  --   array with a length of five in the first dimension, four in the second
  --   dimension, and three in the third dimension would be represented as
  --   [5,4,3]. The flattened array length is equal to the product of the
  --   elements of this list (60 in this example).
  , vectorShape  :: [Int]
  , fieldIndexed :: Bool
  -- | If the datablock field is of 'ProtoBinary' type, this field contains a
  --   "best guess" of the binary data's MIME type.
  , mimeGuess    :: Maybe MIME.Type
  } deriving (Eq, Ord, Show)

-- | Server-internal representation of a datablock.
data DataBlock = DataBlock {
    -- | Canonical name (i.e. not an 'AliasName').
    dbName   :: DataBlockName
    -- | Datablock database ID.
  , dbID     :: Word64
    -- | User ID of datablock owner; 'Nothing' if the datablock was provided by
    --   an automated process.
  , dbOwner  :: Maybe User
    -- | Reference count.
  , dbRefs   :: TVar Int
    -- | Datablock fields.
  , dbFields :: [DataBlockField]
    -- | Index of datablock cell values. 'DataBlockField' values whose
    --   'fieldIndexed' records are 'True' also have their 'fieldNames' as keys
    --   in the outer 'M.Map'; this is accepted as an invariant.
  , dbIndex  :: CellIndex
    -- | A set of all pairs of row offsets from the 'dbMmap' pointer and row
    --   sizes in bytes.
  , dbInds   :: S.Set (Int, Int)
    -- | Pointer to the head of the datablock memory map.
  , dbMmap   :: Ptr Word8
    -- | Size of the datablock memory map in bytes.
  , dbMsize  :: Int
    -- | Number of datablock records.
  , dbRecs   :: Int
  }

-- | Map of datablock names to datablocks.
type DataBlockMap = MVar (M.Map DataBlockName (MVar DataBlock))

-- | Map of field names to field indices, which are maps from unique field
--   values to the 'S.Set' of memory map offset and row byte count pairs.
type CellIndex = M.Map T.Text (M.Map ProtoCell (S.Set (Int, Int)))

-- | This type is used to represent an intermediate value in the evaluation of a
--   datablock record 'Filter'. Representing the intermediate result set as a
--   single top-level 'S.Set' avoids unecessary indirection and simplifies
--   conjuction and disjuction operations.
type RecordReader = Reader CellIndex (S.Set (Int, Int))

-- | Datablock record filter atoms. Each of these constructors represents a
--   predicate on datablock records, allowing API callers to request a subset
--   of the records available in a datablock. The referenced fields must be
--   indexed fields of the queried datablock.
data DataBlockRecordFilter = -- | Equality (== x).
                             FieldEq   T.Text ProtoCell
                             -- | Greater than (> x).
                           | FieldGt   T.Text ProtoCell
                             -- | Greater than or equal to (>= x).
                           | FieldGtEq T.Text ProtoCell
                             -- | Less than (< x).
                           | FieldLt   T.Text ProtoCell
                             -- | Less than or equal to (<= x).
                           | FieldLtEq T.Text ProtoCell
                           deriving (Eq, Ord, Show)

deriving instance Eq (Filter RecordReader)
deriving instance Ord (Filter RecordReader)
deriving instance Show (Filter RecordReader)

instance Filterable RecordReader where
    type Atom RecordReader = DataBlockRecordFilter
    appAtom (FieldEq f v) m = m >> ask >>=
        (\i -> return $ fromMaybe S.empty (M.lookup f i >>= M.lookup v))
    appAtom (FieldGt f v) m = m >> ask >>=
        (\i -> return $ fromMaybe S.empty ((S.unions . M.elems . splitGt v) <$> M.lookup f i))
    appAtom (FieldGtEq f v) m = m >> ask >>=
        (\i -> return $ fromMaybe S.empty ((S.unions . M.elems . splitGtEq v) <$> M.lookup f i))
    appAtom (FieldLt f v) m = m >> ask >>=
        (\i -> return $ fromMaybe S.empty ((S.unions . M.elems . splitLt v) <$> M.lookup f i))
    appAtom (FieldLtEq f v) m = m >> ask >>=
        (\i -> return $ fromMaybe S.empty ((S.unions . M.elems . splitLtEq v) <$> M.lookup f i))
    neg n m = do
        n' <- n
        m' <- m
        return $ S.difference m' n'
    conj l r = do
        l' <- l
        r' <- r
        return $ S.intersection l' r'
    disj l r = do
        l' <- l
        r' <- r
        return $ S.union l' r'

-- | The evaluation of 'Filter RecordReader's requires a special 'ReaderT'
--   environment. This function wraps 'appFilter' with the appropriate
--   environment.
appRecordFilter :: Maybe (Filter RecordReader) -> DataBlock -> S.Set (Int, Int)
appRecordFilter Nothing  d = dbInds d
appRecordFilter (Just f) d = runReader (appFilter f (return S.empty)) (dbIndex d)

-- | Result paging strategy. The results of 'Query' execution may be very
--   large; paging allows the client to incrementally process the result set.
--   Various paging strategies are supported; the coice of paging strategy will
--   depend on the API caller's intention.
data Paging = -- | Linear sampling returns every nth point in the result set.
              --   For n = 4, the first page contains the 0th, 4th, 8th, ...
              --   records, the next page contains the 1st, 5th, 9th, ...
              --   records, etc.
              LinearSampling Int
              -- | Linear chunking returns contiguous pages of n points. For
              --   n = 4, the first page contains the 0th, 1st, 2nd, and 3rd
              --   records, the next page contains the 4th, 5th, 6th, and 7th
              --   records, etc.
            | LinearChunking Int
              -- | Bisection returns 2^n records on the nth page request, i.e.
              --   the points that would appear on the nth level of a balanced
              --   binary tree of the records. For example, let the record set
              --   be [1..10]. The first page contains 4, the next page contains
              --   2 and 8, the next page contains 1, 3, 6, and 9, etc.
            | Bisection
              -- | The result set is returned as a single page.
            | Contiguous
            deriving (Eq, Ord, Show)

-- | A page of query result data.
data Page = Page {
    -- | Payload.
    records :: [[ProtoCell]]
    -- | Continuation.
  , cont    :: Maybe Word64
  } deriving (Eq, Ord, Show)

-- | Result set field selector, consisting of a list of field names acting as
--   either a whitelist or a blacklist.
data FieldSelector =
                     WhiteList [T.Text]
                   | BlackList [T.Text]
                   deriving (Eq, Ord, Show)

-- | Datablock record query, consisting of a 'Filter RecordReader' and optional
--   sorting and paging directives.
data Query = Query {
    -- | The 'DataBlock' to query.
    qDataBlock :: DataBlockName
    -- | Record filter. 'Nothing' returns all rows.
  , qFilter    :: Maybe (Filter RecordReader)
    -- | Optional sorting directive. The 'Bool' indicates whether or not sort
    --   order should be reversed('False' indicates lowest-to-highest according
    --   to the 'Ord' instance). The 'T.Text' is the name of the field to sort
    --   on, which must be indexed.
  , qSort      :: Maybe QuerySort
    -- | Optional field selection directive.
  , qList      :: Maybe FieldSelector
    -- | Query result paging strategy.
  , qPage      :: Paging
  } deriving (Eq, Ord, Show)

newtype QuerySort = QuerySort (Bool, T.Text)
    deriving (Eq, Ord, Show)

-- | A 'Query' result.
data Result =
              Result {
                rFields :: [DataBlockField]
              , rPage   :: Page
              }
            | EmptyResult
            deriving (Eq, Ord, Show)

-- | Caller request for new datablock.
data NewDataBlock = NewDataBlock {
    -- | Canonical name (i.e. not an 'AliasName').
    ndbName :: DataBlockName
    -- | Datablock owner's user ID.
  , ndbOwner :: Word64
    -- | Map of field names to field handling directives. If the 'Bool' is
    --   true, the field will be indexed. If a 'ProtoCellType' is provided, use
    --   of the corresponding parser will be forced.
  , ndbFields :: M.Map T.Text (Bool, Maybe ProtoCellType)
  } deriving (Eq, Show)

-- | User authentication message.
data Auth = Auth {
    username :: T.Text
  , password :: T.Text
  } deriving (Eq, Ord, Show)

-- | System-agnostic representation of a user.
data User = User {
    -- | Trebuchet-native user ID.
    userID   :: Word64
    -- | Username.
  , userName :: T.Text
    -- | Real name.
  , realName :: T.Text
    -- | Email
  , email    :: T.Text
  } deriving (Show)

-- | Assumes that there exists a total bijective function between user IDs and
--   users.
instance Eq User where
    a == b = userID a == userID b

-- | Map of user IDs to users.
type UserMap = MVar (M.Map Word64 (MVar User))

-- | Job argument type.
data JobArgType = -- | Boolean argument.
                  BoolArgType
                  -- | Integral argument.
                | IntArgType
                  -- | Real number argument.
                | RealArgType
                  -- | Arbitrary string argument.
                | StringArgType
                  -- | Enumeration argument.
                | EnumArgType [T.Text]
                  -- | String argument recognized by provided regex.
                | RegexArgType T.Text
                  -- | 'DataBlockName' argument.
                | DataBlockNameArgType
                  -- | Pair of 'DataBlockName' and 'DataBlockField'.
                | DataBlockFieldArgType
                  -- | Datablock tag argument, see 'JobConfig'.
                | DataBlockTagArgType
                  -- | Vector argument, with optional shape requirement.
                | VectorArgType (Maybe [Int]) JobArgType
                deriving (Eq, Ord, Show)

-- | Job argument.
data JobArg = -- | Boolean argument.
              BoolArg   Bool
              -- | Integral argument.
            | IntArg    Word64
              -- | Real number argument.
            | RealArg   Double
              -- | String argument.
            | StringArg T.Text
              -- | Enumeration argument.
            | EnumArg T.Text
              -- | String argument recognized by provided regex.
            | RegexArg T.Text
              -- | 'DataBlockName' argument.
            | DataBlockNameArg DataBlockName
              -- | Pair of 'DataBlockName' and 'DataBlockField'.
            | DataBlockFieldArg DataBlockName DataBlockField
              -- | Datablock tag argument.
            | DataBlockTagArg T.Text
              -- | Vector argument.
            | VectorArg (V.Vector JobArg)
            deriving (Eq, Ord, Show)

-- | Job argument validation abstract syntax tree, describing a simple language
--   for validating 'JobConfig' arguments.
data JobArgVal = -- | Clause stipulating that the given argument is set.
                 JobArg T.Text
                 -- | Validation clause conjuction.
               | JobArgAnd JobArgVal JobArgVal
                 -- | Validation clause disjunction.
               | JobArgOr  JobArgVal JobArgVal
                 -- | Validation clause exclusive disjunction.
               | JobArgXor JobArgVal JobArgVal
                 -- | Validation clause negation.
               | JobArgNot JobArgVal
               deriving (Eq, Ord, Show)

-- | Document this.
data JobParam = JobParam {
    jobParamDispName :: T.Text
  , jobParamKeyName  :: T.Text
  , jobParamDesc     :: Maybe T.Text
  , jobParamDefault  :: Maybe JobArg
  , jobParamArgType  :: JobArgType
  } deriving (Eq, Ord, Show)

-- | A job template, uniquely idenfifying a runnable job and its argument
--   requirements.
data JobTemplate = JobTemplate {
    -- | Unique identifier of a job template. Used internally.
    jobTemplateId     :: Word64
    -- | Unique human readable (but invariant) job identifier.
  , jobTemplateKey    :: T.Text
    -- | Job template display name. More descriptive than 'jobTemplateKey' and
    --   potentially subject to change.
  , jobTemplateName   :: T.Text
  , jobTemplateDesc   :: Maybe T.Text
    -- | Job template parameter set, specifiying which arguments /may/ be
    --   present, with optional default vaule.
  , jobTemplateParams :: M.Map T.Text JobParam
    -- | Job argument constraints.
  , jobTemplateConstr :: Maybe JobArgVal
  } deriving (Eq, Ord, Show)

-- | A runnable job configuration. The validity of a type member's 'jobArgs'
--   with respect to the given 'JobTemplate' is taken as an invariant, i.e.
--   constructors are assumed to have performed this check correctly.
data JobConfig = JobConfig {
    -- | Optional job configuration name.
    jobConfigName       :: Maybe T.Text
    -- | Job template.
  , jobConfigTemplateId :: Word64
    -- | Job arguments.
  , jobConfigArgs       :: M.Map T.Text JobArg
    -- | The datablocks to be fed to the job, each with a unique(within one
    --   'JobConfig') textual tag and optional 'query' applied.
  , jobConfigDataBlocks :: [(Maybe T.Text, DataBlockName, Maybe Query)]
  } deriving (Eq, Ord, Show)

-- | Job status.
data JobStatus = -- | Job quick win.
                JobSuccess {
                  completionTime :: UTCTime
                }
                -- | Job cancelation.
              | JobCanceled {
                  canceler   :: User  -- ^ User ID of the canceler.
                , cancelTime :: UTCTime -- ^ Cancelation time.
                }
                -- | Job execution error.
              | JobFailure {
                  errorString :: T.Text  -- ^ Error string, i.e. stderr.
                , errorTime   :: UTCTime -- ^ Job failure time.
                }
              deriving (Eq, Show)

-- | A instance of a job that is executing or has executed. The existence of a
--   member of this type for a job indicates that a Trebuchet server has
--   validated the arguments, recorded the job execution event in the database,
--   and notified the job server.
data Job = Job {
    -- | Unique job ID.
    jobID         :: Word64
    -- | Job executor.
  , jobExecutorUsername :: T.Text
    -- | Job configuration.
  , jobConfig     :: JobConfig
    -- | Job start time.
  , jobStart      :: UTCTime
    -- | Job status. 'Nothing' if the job is still executing.
  , jobStatus     :: Maybe JobStatus
    -- | Job results. 'Nothing' if the job is still executing.
  , jobResult     :: Maybe [DataBlockName]
  } deriving (Eq, Ord, Show)

-- | A request for a worker to execute a job.
data JobReq = JobReq {
    -- | 'jobID' of the 'Job' we're requesting.
    jobReqID :: Word64
  , -- | 'jobTemplateKey' of the 'JobTemplate' of the 'Job' we're requesting.
    --   This identifies the "job image" to the worker.
  , jobReqTemplateKey :: T.Text
    -- | 'jobConfig' of the 'Job' we're requesting.
  , jobReqConfig      :: JobConfig
  } deriving (Eq, Ord, Show)

type JobTemplateMap = MVar (M.Map T.Text (MVar JobTemplate))

type JobConfigMap = MVar (M.Map T.Text (MVar JobConfig))

type JobMap = MVar (M.Map Word64 (MVar Job))

-- | Document this.
data ClientError = ClientError ClientErrorCode T.Text

-- | Document this.
data ClientErrorCode = CEMissingSessionCookie | CEInvalidSessionCookie | CEUserNotFound | CEInvalidCSV

lookupMap :: Ord k => k -> M.Map k a -> M.Map k a
lookupMap k m = case M.lookup k m of Nothing  -> M.empty
                                     (Just a) -> M.singleton k a

splitGt :: Ord k => k -> M.Map k a -> M.Map k a
splitGt = (snd .) . M.split

splitGtEq :: Ord k => k -> M.Map k a -> M.Map k a
splitGtEq k m = let (_, mv, g) = M.splitLookup k m
                in case mv of Nothing  -> g
                              (Just v) -> M.insert k v g

splitLt :: Ord k => k -> M.Map k a -> M.Map k a
splitLt = (fst .) . M.split

splitLtEq :: Ord k => k -> M.Map k a -> M.Map k a
splitLtEq k m = let (l, mv, _) = M.splitLookup k m
                in case mv of Nothing  -> l
                              (Just v) -> M.insert k v l
