{-# LANGUAGE CPP, DataKinds, DeriveGeneric, TypeFamilies, TypeOperators, OverloadedStrings, LambdaCase #-}

module Treb.Demo where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad

import Data.Word

import System.Environment

import System.IO.MMap

import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.Text as T

import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Servant

import ProtoDB.Reader
import ProtoDB.Types

import Treb.Types
import Treb.Filter
import Treb.JSON
import Treb.Index

fivemins :: Int
fivemins = 5 * 60 * 1000000

liftMServ :: IO (Either String r) -> ExceptT ServantErr IO r
liftMServ m = do
    e <- liftIO m
    case e of (Right v) -> return v
              (Left e)  -> throwE (err404 {errBody = (B.pack e)})

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = xs' : chunk n xs''
    where (xs', xs'') = splitAt n xs

dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN 0 xs = xs
dropN 1 _  = []
dropN n (x:xs) = x : dropN n (drop (n-1) xs)

linSamp :: Int -> [a] -> [[a]]
linSamp 0 xs     = [xs]
linSamp 1 xs     = [xs]
linSamp n xs = map (dropN n) [drop i xs | i <- [0..(n-1)]]

setPage :: Maybe Word64 -> [[ProtoCell]] -> Page
setPage c rs = Page rs c

type TrebAPI = "db" :> Get '[JSON] [DataBlock]
          :<|> "db" :> ReqBody '[JSON] (Filter (M.Map DataBlockName DataBlock)) :> Post '[JSON] [DataBlock]
          :<|> "q"  :> ReqBody '[JSON] Query :> Post '[JSON] Result
          :<|> "p"  :> Capture "pid" Word64 :> Get '[JSON] Page

data TrebState = TrebState {
    dataBlockMap :: DataBlockMap
  , pageMap      :: TVar (I.IntMap ((Maybe Int), (ExceptT ServantErr IO Page)))
  , pageIndex    :: TVar Int
  }

parseArgs :: [String] -> IO [DataBlock]
parseArgs []         = return []
parseArgs (fn:is:as) = (:) <$> mkFileDataBlock fn (read is) <*> parseArgs as
parseArgs _          = error "usage: datablock1 [\"col1\", \"col2\", ...] ..."

mkFileDataBlock :: FilePath -> [T.Text] -> IO DataBlock
mkFileDataBlock fn fs = do
    dbbs <- B.readFile fn
    let mkDataBlock rdb rec ci = do
            rc <- newTVarIO 0
            (ptr, rs, _, _) <- mmapFilePtr fn ReadOnly Nothing
            return $ DataBlock (AdHocName (rdbTitle rdb) T.empty)
                               0
                               Nothing
                               rc
                               (map mkDataBlockField (rdbFields rdb))
                               ci
                               ptr
                               rs
                               rec
        mkDataBlockField rf = DataBlockField (rfTitle rf)
                                             (rfType rf)
                                             (rfVector rf)
                                             (elem (rfTitle rf) fs)
                                             Nothing
    case genIndex dbbs fs of (Right (rdb, rec, ci)) -> mkDataBlock rdb rec ci
                             (Left err)             -> error err

insDataBlock :: DataBlockMap -> DataBlock -> IO DataBlockMap
insDataBlock dbm db = do
    dbm' <- takeMVar dbm
    dbl <- newMVar db
    putMVar dbm (M.insert (dbName db) dbl dbm')
    return dbm

listDataBlockHandler :: TrebState -> ExceptT ServantErr IO [DataBlock]
listDataBlockHandler ts = do
    dbm <- liftIO $ readMVar (dataBlockMap ts)
    liftIO $ mapM readMVar (map snd $ M.toList dbm)

filterDataBlockHandler :: TrebState
                       -> Filter (M.Map DataBlockName DataBlock)
                       -> ExceptT ServantErr IO [DataBlock]
filterDataBlockHandler ts f = do
    dbm  <- liftIO $ readMVar (dataBlockMap ts)
    dbmp <- liftIO $ mapM readMVar dbm
    let rm = appFilter f dbmp
    return (map snd $ M.toList rm)

queryDataBlockHandler :: TrebState -> Query -> ExceptT ServantErr IO Result
queryDataBlockHandler ts q = do
    dbm <- liftIO $ readMVar (dataBlockMap ts)
    let runQuery :: DataBlock -> Maybe (Bool, T.Text) -> ExceptT ServantErr IO [S.Set (Int, Int)]
        runQuery db Nothing           = return . (:[]) $ appRecordFilter (qFilter q) db
        runQuery db (Just (False, f)) = return . sortResult db f $ appRecordFilter (qFilter q) db
        runQuery db (Just (True, f))  = return . rSortResult db f $ appRecordFilter (qFilter q) db
        sortResult :: DataBlock -> T.Text -> S.Set (Int, Int) -> [S.Set (Int, Int)]
        sortResult db f s = case M.lookup f (dbIndex db)
                                  of Nothing  -> []
                                     (Just m) -> map (S.intersection s . snd) $ M.toList m
        rSortResult :: DataBlock -> T.Text -> S.Set (Int, Int) -> [S.Set (Int, Int)]
        rSortResult db f s = case M.lookup f (dbIndex db)
                                  of Nothing -> []
                                     (Just m) -> (map (S.intersection s . snd) . reverse) (M.toList m)
        setsRows :: DataBlock -> [S.Set (Int, Int)] -> IO (Either String [[ProtoCell]])
        setsRows db = ((concat <$>) . sequenceA <$>) .
            mapM (fetchRowSet (dbMmap db) (dbRefs db) (map fieldType (dbFields db)))
        mkPages :: DataBlock -> [S.Set (Int, Int)] -> ExceptT ServantErr IO Result
        mkPages db s = do
            let prune = case (qList q) of Nothing               -> id
                                          (Just (WhiteList fs)) -> map (inds (map fst (whlst fs)))
                                          (Just (BlackList fs)) -> map (inds (map fst (bllst fs)))
                whlst fs = (filter (((flip elem) fs) . snd) . zip [0..] . map fieldName) (dbFields db)
                bllst fs = (filter (not . ((flip elem) fs) . snd) . zip [0..] . map fieldName) (dbFields db)
                dbflist = case (qList q)
                              of Nothing -> (dbFields db)
                                 (Just (WhiteList fs)) -> filter (((flip elem) fs) . fieldName) (dbFields db)
                                 (Just (BlackList fs)) -> filter (not . ((flip elem) fs) . fieldName) (dbFields db)
                sample (LinearSampling n) = linSamp n s
                sample (LinearChunking n) = chunk n s
                sample _                  = [s]
                mkPagesT :: [ExceptT ServantErr IO [[ProtoCell]]] -> STM [ExceptT ServantErr IO Page]
                mPagesT (p:[]) = do
                    m <- readTVar (pageMap ts)
                    i <- readTVar (pageIndex ts)
                    let p' = setPage Nothing <$> p
                        m' = I.insert i (Nothing, p') m
                        i' = i + 1
                    writeTVar (pageMap ts) m'
                    writeTVar (pageIndex ts) i'
                    return [p']
                mkPagesT (p:ps) = do
                    m <- readTVar (pageMap ts)
                    i <- readTVar (pageIndex ts)
                    let i' = i + 1
                        p' = setPage (Just (fromIntegral i')) <$> p
                        m' = I.insert i (Just i', p') m
                    writeTVar (pageMap ts) m'
                    writeTVar (pageIndex ts) i'
                    (p':) <$> (mkPagesT ps)
                startReaper :: Word64 -> ExceptT ServantErr IO ()
                startReaper i = do
                    r <- liftIO $ registerDelay fivemins
                    liftIO $ forkIO (reaper ts (fromIntegral i) r)
                    return ()
            (i:_) <- liftIO $ (atomically . mkPagesT . map ((prune <$>) . liftMServ . setsRows db)) (sample (qPage q))
            i'    <- i
            case (cont i') of Nothing -> return ()
                              (Just i) -> startReaper i
            return $ Result dbflist i'
    case M.lookup (qDataBlock q) dbm
         of Nothing  -> throwE $ err404 { errBody = "no such datablock" }
            (Just d) -> liftIO (readMVar d) >>= (\d' -> (runQuery d' (qSort q)) >>= mkPages d')

reaper :: TrebState -> Int -> TVar Bool -> IO ()
reaper ts i t = atomically $ readTVar t >>= (\r -> if r then reap i else retry)
    where reap r = do
            m <- readTVar (pageMap ts)
            let r' = I.lookup r m
            case r' of Nothing              -> return ()
                       (Just (Nothing, _))  -> writeTVar (pageMap ts) (I.delete r m)
                       (Just (Just r'', _)) -> writeTVar (pageMap ts) (I.delete r m) >> reap r''

pageHandler :: TrebState -> Word64 -> ExceptT ServantErr IO Page
pageHandler ts i = let fetchP = do
                            m <- readTVar (pageMap ts)
                            return (snd <$> I.lookup (fromIntegral i) m)
                   in (liftIO (atomically fetchP))
                       >>= \case Nothing  -> throwE $ err404 {errBody = "no such page"}
                                 (Just p) -> p

demomain :: IO ()
demomain = do
    fns <- getArgs
    dbs <- parseArgs fns
    init_dbm <- newMVar M.empty
    dbm <- foldM insDataBlock init_dbm dbs
    pgm <- newTVarIO I.empty
    pgi <- newTVarIO 0
    let ts = TrebState dbm pgm pgi
        server :: Server TrebAPI
        server = listDataBlockHandler ts
            :<|> filterDataBlockHandler ts
            :<|> queryDataBlockHandler ts
            :<|> pageHandler ts
        trebAPI :: Proxy TrebAPI
        trebAPI = Proxy
        app = serve trebAPI server
    run 8080 $ simpleCors app
