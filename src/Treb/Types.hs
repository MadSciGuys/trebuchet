{-|
Module:      Treb.Types
Description: Trebuchet types.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , TypeSynonymInstances
           , StandaloneDeriving
           , LambdaCase
           #-}

module Treb.Types where

import Control.Concurrent.MVar

import Control.Concurrent.STM.TVar

import Control.Monad.Reader

import qualified Data.ByteString as B

import Data.List (foldl')

import Data.Word

import Data.Maybe (isJust, fromMaybe)

import qualified Data.Map as M

import qualified Data.Set as S

import qualified Data.Text as T

import Data.Time

import qualified Data.Vector as V

import Foreign.Ptr

import ProtoDB.Types

import System.Posix.Types

import Treb.Filter
import Treb.BadRegex

-- | Datablock name. Each datablock has at least one unique element of this
--   type.
data DataBlockName = AdHocName T.Text -- ^ Ad-hoc (i.e. user provided)
                                      --   datablock.
                   | RecipeName {
                       compoundName :: T.Text   -- ^ Compound name.
                     , recipeNames  :: [T.Text] -- ^ Recipe names in topological
                                                --   order.
                     }                -- ^ Recipe datablock.
                   | JobResultName Word64 T.Text -- ^ Job execution result.
                   | AliasName T.Text            -- ^ Datablock alias.
                   deriving (Eq, Ord, Show)

-- | Datablock name type.
data DataBlockNameType = AdHocType
                       | RecipeType
                       | JobResultType
                       | AliasType
                       deriving (Eq, Ord, Show)

-- | Get the datablock name type.
dataBlockNameType :: DataBlockName -> DataBlockNameType
dataBlockNameType (AdHocName _)       = AdHocType
dataBlockNameType (RecipeName _ _)    = RecipeType
dataBlockNameType (JobResultName _ _) = JobResultType
dataBlockNameType (AliasName _)       = AliasType

isAdHocType :: DataBlockName -> Bool
isAdHocType (AdHocName _) = True
isAdHocType _             = False

isRecipeType :: DataBlockName -> Bool
isRecipeType (RecipeName _ _) = True
isRecipeType _                = False

isJobResultType :: DataBlockName -> Bool
isJobResultType (JobResultName _ _) = True
isJobResultType _                   = False

isAliasType :: DataBlockName -> Bool
isAliasType (AliasName _) = True
isAliasType _             = False

-- | Datablock filter atoms.
data DataBlockFilter = -- | Match datablocks with a name of a specific type.
                       NameType DataBlockNameType
                       -- | Match a datablock name exactly.
                     | NameExact DataBlockName
                       -- | Match all datablock names recognized by the
                       --   provided regular expression.
                     | NameRegex T.Text
                       -- | Match a datablock hash exactly.
                     | HashExact Integer
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
    appAtom (NameRegex e) = let compMatch (AdHocName n) = n `badRegexBool` e
                                compMatch (RecipeName cn rns) = (cn `badRegexBool` e) || (and $ map (`badRegexBool` e) rns)
                                compMatch (JobResultName i n) = ((T.pack (show i)) `badRegexBool` e) || (n `badRegexBool` e)
                                compMatch (AliasName n) = n `badRegexBool` e
                            in M.filter (compMatch . dbName)
    appAtom (HashExact h) = M.filter ((h ==) . dbHash)
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

-- | Name, type, and array description of a datablock field.
data DataBlockField = DataBlockField {
    fieldName    :: T.Text
  , fieldType    :: ProtoType
  -- | If the field is a scalar, this will equal the empty list. If it is a
  --   vector, it will contain a list of integers describing the maximum length
  --   in each dimension in row-major order. For example, a three dimensional
  --   array with a length of five in the first dimension, four in the second
  --   dimension, and three in the third dimension would be represented as
  --   [5,4,3]. The flattened array length is equal to the product of the
  --   elements of this list (60 in this example).
  , vectorShape  :: [Int]
  , fieldIndexed :: Bool
  } deriving (Eq, Ord, Show)

-- | Internal representation of a datablock.
data DataBlock = DataBlock {
    -- | Canonical name (i.e. not an 'AliasName').
    dbName   :: DataBlockName
    -- | SHA1 hash.
  , dbHash   :: Integer
    -- | User ID of datablock owner.
  , dbOwner  :: Maybe User
    -- | Reference count.
  , dbRefs   :: TVar Int
    -- | Datablock fields.
  , dbFields :: [DataBlockField]
    -- | Index of datablock cell values. 'DataBlockField' values whose
    --   'fieldIndexed' records are 'True' also have their 'fieldNames' as keys
    --   in the outer 'M.Map'; this is accepted as an invariant.
  , dbIndex  :: CellIndex
    -- | Pointer to the head of the datablock memory map.
  , dbMmap   :: Ptr Word8
    -- | Size of the datablock memory map in bytes.
  , dbMsize  :: Int
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

-- | Datablock record filter atoms.
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

-- | The evaluation of 'Filter DataBlockRecordFilter's requires a special
--   'ReaderT' environment. This function wraps 'appFilter' with the appropriate
--   environment.
appRecordFilter :: Filter RecordReader -> DataBlock -> S.Set (Int, Int)
appRecordFilter f d = runReader (appFilter f (return S.empty)) (dbIndex d)

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
    -- | User token, only present if user is logged in.
  , token    :: MVar B.ByteString
  }

-- | Assumes that there exists a total bijective function between user IDs and
--   users.
instance Eq User where
    a == b = (userID a) == (userID b)

-- | Displays only user metadata.
instance Show User where
    show (User i un rn e _) = concat
        [ "User {userID = "
        , show i
        , ", userName = "
        , show un
        , ", realName = "
        , show rn
        , ", emails = "
        , show e
        , "}"
        ]

-- | Map of user IDs to users.
type UserMap = MVar (M.Map Word64 (MVar User))

-- | Job argument type.
data JobArgType = BoolArgType                 -- ^ Boolean argument.
                | IntArgType                  -- ^ Integral argument.
                | RealArgType                 -- ^ Real number argument.
                | StringArgType               -- ^ String argument.
                | VectorArgType (Maybe [Int]) -- ^ Vector argument, with
                                              --   optional shape requirement.
                deriving (Eq, Ord, Show)

-- | Job argument.
data JobArg = BoolArg   Bool              -- ^ Boolean argument.
            | IntArg    Integer           -- ^ Integral argument.
            | RealArg   Double            -- ^ Real number argument.
            | StringArg T.Text            -- ^ String argument.
            | VectorArg (V.Vector JobArg) -- ^ Vector argument.
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

-- | A job template, uniquely idenfifying a runnable job and its argument
--   requirements.
data JobTemplate = JobTemplate {
    -- | Job template name.
    jobTemplateName   :: T.Text
    -- | Job template parameter set, specifiying which arguments /may/ be
    --   present.
  , jobTemplateParams :: M.Map T.Text JobArgType
    -- | Job argument constraints.
  , jobTemplateConstr :: JobArgVal
  } deriving (Eq, Ord, Show)

-- | A runnable job configuration. The validity of a type member's 'jobArgs'
--   with respect to the given 'JobTemplate' is taken as an invariant, i.e.
--   constructors are assumed to have performed this check correctly.
data JobConfig = JobConfig {
    -- | Optional job configuration name.
    jobConfigName :: Maybe T.Text
    -- | Job template.
  , jobTemplate   :: JobTemplate
    -- | Job arguments.
  , jobArgs       :: M.Map T.Text JobArg
    -- | The datablocks to be fed to the job, each with an optional query
    --   applied.
  , jobDataBlocks :: [(DataBlockName, Maybe (Filter RecordReader))]
  } deriving (Eq, Ord, Show)

-- | Job error.
data JobError = -- | Job cancelation.
                JobCanceled {
                  canceler   :: User  -- ^ User ID of the canceler.
                , cancelTime :: UTCTime -- ^ Cancelation time.
                }
                -- | Job execution error.
              | JobFailure {
                  errorString :: T.Text  -- ^ Error string.
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
  , jobExecutorID :: User
    -- | Job configuration.
  , jobConfig     :: JobConfig
    -- | Job start time.
  , jobStart      :: UTCTime
    -- | Job status. 'Nothing' if the job is still executing.
  , jobStatus     :: Maybe (Either JobError UTCTime)
    -- | Job results. 'Nothing' if the job is still executing.
  , jobResult     :: Maybe DataBlockName
  } deriving (Eq, Show)

type JobTemplateMap = MVar (M.Map T.Text (MVar JobTemplate))

type JobConfigMap = MVar (M.Map T.Text (MVar JobConfig))

type JobMap = MVar (M.Map Word64 (MVar Job))

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
