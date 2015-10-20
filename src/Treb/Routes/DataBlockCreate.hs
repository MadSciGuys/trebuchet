{-|
Module:      Treb.Routes.DataBlockCreate
Description: Trebuchet DataBlockCreate route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, QuasiQuotes #-}

--module Treb.Routes.DataBlockCreate ( DataBlockCreateH, dataBlockCreateH ) where
module Treb.Routes.DataBlockCreate where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString as A
import qualified Hasql as H
import Control.Monad.IO.Class
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Either ()
import Control.Monad.Identity
import Data.CSV.Conduit
import Data.ProtoBlob
import ProtoDB.Parser
import ProtoDB.Types
import ProtoDB.Writer
import Treb.Routes.Helpers
import Treb.Routes.Types
import Treb.Types
import Treb.Routes.FileUpload

---- Route-Specific Type ----
--    type DataBlockCreateH =
--        "datablock" :> "create"
--            :> ReqBody '[JSON] DataBlockCreateMsg
--            :> DrupalAuth
--            :> Post '[JSON] (NoWrapEither DataBlockFileUploadMsg DataBlockMetadataMsg)
--    
--    dataBlockCreateH :: TrebServer DataBlockCreateH
--    dataBlockCreateH msg@(DataBlockCreateMsg name maybeFields maybeRecords) = drupalAuth $ do
--        user <- getCurrentUser
--        maybe
--            (do
--                uri <- newFileUpload (dataBlockCreateFileUpload msg)
--                return $ NoWrapEither $ Left $ DataBlockFileUploadMsg uri)
--            (\records ->
--                maybe
--                    (serverError "DataBlock creation without explicit fields is unimplemented. TODO.")
--                    (\fields -> do
--                        let protoFields = [ WritableField (fieldName field) (fieldType field) (vectorShape field) | field <- fields ]
--                        writeUserDataBlock user name protoFields (V.foldl' (flip (++)) [] $ V.reverse $ V.map V.toList records)
--                        Identity dbId <- queryPG H.singleEx $
--                            [H.stmt|insert into "datablock"
--                                    ( datablock_name
--                                    , datablock_source )
--                                    values (('ad_hoc', ?, NULL, NULL, NULL), ('user', ?, NULL))
--                                    returning id |] name (userName user)
--                        return $ NoWrapEither $ Right $ DataBlockMetadataMsg dbId (AdHocName name (userName user)) fields (V.length records))
--                    maybeFields
--                )
--            maybeRecords
--    
--    dataBlockCreateFileUpload :: DataBlockCreateMsg -> B.ByteString -> TrebServerBase DataBlockMetadataMsg
--    dataBlockCreateFileUpload (DataBlockCreateMsg name givenFields _) uploadContent = do
--        user <- getCurrentUser
--        either
--            (clientError CEInvalidCSV . T.pack . show)
--            (\csv -> do
--                -- Check consistency of uploaded CSV and the user-specified set of fields.
--                fields <- maybe
--                    (serverError "CSV upload without explicit fields is unimplemented. TODO.")
--                    -- TODO: Check consistency of values in CSV. --
--                    return
--                    givenFields
--                -- Calculate protocol buffer fields from Trebuchet fields.
--                protoFields <- mapM (\field ->
--                    if vectorShape field /= [] then
--                        clientError CEInvalidCSV "CSV may not contain vector fields."
--                    else
--                        return $ WritableField (fieldName field) (fieldType field) []) fields
--                protoCells <- either
--                            (clientError CEInvalidCSV . T.pack)
--                            return
--                            (parseProtoCSV csv)
--                writeUserDataBlock user name protoFields protoCells
--                -- TODO: Write entry in PostgreSQL here.
--                return $ DataBlockMetadataMsg 0 (AdHocName name (userName user)) fields (V.length csv))
--            (decodeCSV defCSVSettings uploadContent)
--    
--    parseProtoCSV :: V.Vector (V.Vector B.ByteString) -> Either String [ProtoCell]
--    parseProtoCSV =
--        V.foldM (\xs row ->
--            fmap (++ xs) $ 
--                V.foldM
--                    (\ys cell ->
--                        either
--                            Left
--                            (maybe
--                                (Left "Failed to parse at least one CSV cell.")
--                                (Right . (:ys)))
--                            $ A.parseOnly parseProtoCell cell) [] row) []
--    
--    writeUserDataBlock :: User
--                       -> T.Text
--                       -> [WritableField]
--                       -> [ProtoCell]
--                       -> TrebServerBase ()
--    writeUserDataBlock user name fields records =
--        -- TODO: Add ad-hoc user datablock directory path to TrebEnv in Treb.Types and add a corresponding argument handler in Treb.Config.
--        liftIO $ BL.writeFile ("user_datablocks/" ++ T.unpack name)
--            $ runPutBlob $ writeDB $ WritableDB name fields records
