{-|
Module:      Treb.Index
Description: Datablock Indexer
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitka@its.jnj.com
Stability:   Provisional
Portability: POSIX

Functions for generating and querying read-only in-memory indices of Datablocks.
-}

{-# LANGUAGE BangPatterns #-}

module Treb.Index where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Exception.Base

import Control.Monad

import Data.Word

import Data.List

import Foreign.Ptr

import qualified Data.ByteString.Unsafe     as B
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text as T

import qualified Data.Map as M

import qualified Data.Set as S

import Data.ProtoBlob

import ProtoDB.Types
import ProtoDB.Reader

import Treb.Types

-- | Assume input list of indices is sorted lowest to highest.
inds :: [Int] -> [a] -> [a]
inds = go 0
    where go _ [] _ = []
          go _ _ [] = []
          go i (i':is) (a:as)
                | i < i'  = go (i+1) (i':is) as
                | i == i' = a : go (i+1) is as
                | i > i'  = error "inds: input list not sorted."

map1t3 :: (a -> d) -> (a, b, c) -> (d, b, c)
map1t3 f (a, b, c) = (f a, b, c)

genIndex :: BL.ByteString -- ^ Lazy ByteString of Datablock Payload.
         -> [T.Text]      -- ^ Names of fields to index.
         -> Either String (ReadDB, Int, S.Set (Int, Int), CellIndex)
genIndex p fs = do
    (rdb, rs) <- lazyForceReadDBIndex p
    let fis :: [(Int, T.Text)]
        fis = (filter (((flip elem) fs) . snd) . zip [0..] . map rfTitle) (rdbFields rdb)
        rs' :: [(Either String ([(T.Text, ProtoCell)], Int, Int))]
        rs' = map (fmap (map1t3 (zip (map snd fis) . inds (map fst fis)))) rs
        insRow :: (Int, S.Set (Int, Int), CellIndex) -> Either String ([(T.Text, ProtoCell)], Int, Int) -> Either String (Int, S.Set (Int, Int), CellIndex)
        insRow (!l, !o, !m) (Right (cs, !st, !sz)) = Right (l+1, S.insert (st, sz) o, foldl' (insCell (st, sz)) m cs)
        insRow _            (Left e)               = Left e
        insCell :: (Int, Int) -> CellIndex -> (T.Text, ProtoCell) -> CellIndex
        insCell s !m (!f, !v) = M.alter (insSet s v) f m
        insSet :: (Int, Int)
               -> ProtoCell
               -> Maybe (M.Map ProtoCell (S.Set (Int, Int)))
               -> Maybe (M.Map ProtoCell (S.Set (Int, Int)))
        insSet s !v Nothing = Just $ M.singleton v (S.singleton s)
        insSet s !v (Just m) = Just $ M.alter (Just . maybe (S.singleton s) (S.insert s)) v m
    (len, offsets, cellIndex) <- foldM insRow (0, S.empty, M.empty) rs'
    return (rdb, len, offsets, cellIndex)

fetchRow :: Ptr Word8       -- ^ Pointer to head of datablock memory map.
         -> TVar Int        -- ^ DataBlock reference count.
         -> [ProtoCellType] -- ^ The type and order of cells in the row.
         -> (Int, Int)      -- ^ Target row offset and size.
         -> IO (Either String [ProtoCell])
fetchRow ptr rc ts (st, sz) = parseRow <$> fetchBS
    where fetchBS = bracketOnError incRC
                                   (const decRC)
                                   (const $ B.unsafePackCStringFinalizer (plusPtr ptr st) sz decRC)
          incRC = atomically (modifyTVar' rc (+1))
          decRC = atomically (modifyTVar' rc ((-) 1))
          parseRow = runGetBlob (readRow ts) . BL.fromStrict

fetchRowSet :: Ptr Word8
            -> TVar Int
            -> [ProtoCellType]
            -> S.Set (Int, Int)
            -> IO (Either String [[ProtoCell]])
fetchRowSet ptr rc ts s = parseRows <$> fetchBSs
    where fetchBSs = bracketOnError incPreserveRC
                                    restoreRC
                                    (const $ mapM fetchBS (S.toList s))
          fetchBS (st, sz) = B.unsafePackCStringFinalizer (plusPtr ptr st) sz decRC
          incPreserveRC = atomically $ readTVar rc >>= (\v -> writeTVar rc (v + S.size s) >> return v)
          restoreRC = atomically . writeTVar rc
          decRC = atomically $ modifyTVar' rc ((-) 1)
          parseRows = mapM $ runGetBlob (readRow ts) . BL.fromStrict
