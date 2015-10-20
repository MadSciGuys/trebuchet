{-|
Module:      Treb.Routes.DataBlockGetFilter
Description: Trebuchet DataBlockGetFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

--module Treb.Routes.DataBlockGetFilter ( DataBlockGetFilterH, dataBlockGetFilterH ) where
module Treb.Routes.DataBlockGetFilter where

import Treb.Routes.Types
import qualified Data.Text as T

---- Route-Specific Type ----
--type DataBlockGetFilterH =
--    "datablock" :> Capture "datablock_id" DataBlockId :> "filter"
--        :> ReqBody '[JSON] DataBlockGetFilterMsg
--        :> Post '[JSON] DataBlockRecords

--dataBlockGetFilterH :: TrebServer DataBlockGetFilterH
--dataBlockGetFilterH dbId msg = do
    -- db <- getDataBlock dbId
    -- if isWhiteList msg then
    --     columns msg
    -- columns msg

--data DataBlockGetFilterMsg = DataBlockGetFilterMsg 
--    { columns      :: [T.Text]
--    , isWhiteList  :: Bool
--    , recordFilter :: DataBlockRecordFilter }
--
--instance FromJSON DataBlockGetFilterMsg where
--    parseJSON (Object v) =
--        (DataBlockGetFilterMsg <$> v .: "column_whitelist"
--                               <*> pure True
--                               <*> v .: "record_filter") <|>
--        (DataBlockGetFilterMsg <$> v .: "column_blacklist"
--                               <*> pure False
--                               <*> v .: "record_filter")

---------------
-- Game Plan --
---------------
-- 
-- This is HTTP handler that will service the graphing code in the visualization module. Needless to say, this is not complete
-- and everything is open to change at any moment.
--
-- I see this being interacted with as follows:
--
--   Demos has in hand the structured datablock name he wants record data for. He sends this to /datablock_records/filter, along
--   with either a whitelist or a blacklist of datablock columns that he would like returned and a record filter. We return the
--   total number of pages, an array of column describing objects like those in the ToJSON DataBlock instance, and an ephemeral
--   query identifier which is then later used to get each page. Demos would make GET requests to something like
--   /datablock_records/:query_id/1, which would return N arrays of M-length arrays of primitive JSON values
--   (strings and numbers) where N is the number of records in that page of the datablock subset and M is the number of columns
--   determined by the blacklist/whitelist.
