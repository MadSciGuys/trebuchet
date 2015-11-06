{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Treb.DB.Statements where

import qualified Hasql as H
import qualified Hasql.Postgres as HP
import qualified Data.Text as T
import Data.Word
import Treb.Types

mkDataBlock :: DataBlockName -> H.Stmt HP.Postgres
mkDataBlock = appDataBlockName [H.stmt|insert into "datablock" ("datablock_name") values ((?, ?, ?, ?, ?, ?, ?)) returning "id"|]

appDataBlockName :: ( T.Text 
                   -> Maybe T.Text
                   -> Maybe T.Text
                   -> Maybe T.Text
                   -> Maybe [T.Text]
                   -> Maybe T.Text
                   -> Maybe Word64
                   -> stmt )
                 -> DataBlockName
                 -> stmt
appDataBlockName stmt_f dbn =
    case dbn of
        AdHocName name uploader ->
            stmt_f "ad_hoc" (Just name) (Just uploader) Nothing Nothing Nothing Nothing
        RecipeName compound recipes pipeline ->
            stmt_f "recipe" Nothing Nothing (Just compound) (Just recipes) (Just pipeline) Nothing
        JobResultName jobId name ->
            stmt_f "job_result" (Just name) Nothing Nothing Nothing Nothing (Just jobId)
        AliasName name ->
            stmt_f "alias" (Just name) Nothing Nothing Nothing Nothing Nothing
