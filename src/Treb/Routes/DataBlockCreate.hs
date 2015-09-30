{-|
Module:      Treb.Routes.DataBlockCreate
Description: Trebuchet DataBlockCreate route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockCreate ( DataBlockCreateH, dataBlockCreateH ) where

import Treb.Routes.Helpers
import Treb.Routes.Types
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString as A
import Data.ProtoBlob
import ProtoDB.Parser
import ProtoDB.Types
import ProtoDB.Writer
import Control.Monad.Trans.Either
import Treb.Combinators
import Data.CSV.Conduit
import Treb.Types
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Aeson

---- Route-Specific Type ----
type DataBlockCreateH =
    "datablock" :> "create"
        :> ReqBody '[JSON] DataBlockCreateMsg
        :> DrupalAuth
        :> Post '[JSON] (NoWrapEither DataBlockFileUploadMsg DataBlockMetadataMsg)

dataBlockCreateH :: TrebServer DataBlockCreateH
dataBlockCreateH msg@(DataBlockCreateMsg name maybeFields maybeRecords) = drupalAuth $ \user -> do
    maybe
        (do
            uri <- fileUpload (dataBlockCreateFileUpload user msg)
            return $ NoWrapEither $ Left $ DataBlockFileUploadMsg uri)
        (\records -> do
            -- TODO: Write ProtoBlob here.
            -- TODO: Write entry in PostgreSQL here.
            maybe
                (serverError "DataBlock creation without explicit fields is unimplemented. TODO.")
                (\fields -> return $ NoWrapEither $ Right $ DataBlockMetadataMsg 0 (AdHocName name (userName user)) fields (V.length records))
                maybeFields
            )
        maybeRecords

dataBlockCreateFileUpload :: User -> DataBlockCreateMsg -> TrebServer (FileUploadH DataBlockMetadataMsg)
dataBlockCreateFileUpload origUser (DataBlockCreateMsg name givenFields _) uploadId uploadContent = drupalAuth $ \user -> do
    if userName origUser /= userName user then
        serverError "Uploader is not the initiator of this DataBlock creation transaction."
    else
        either
            (clientError CEInvalidCSV . T.pack . show)
            (\csv -> do
                -- Check consistency of uploaded CSV and the user-specified set of fields.
                fields <- maybe
                    (serverError "CSV upload without explicit fields is unimplemented. TODO.")
                    -- TODO: Check consistency of values in CSV. --
                    return
                    givenFields
                -- Calculate protocol buffer fields from Trebuchet fields.
                protoFields <- mapM (\field -> do
                    if vectorShape field /= [] then
                        clientError CEInvalidCSV "CSV may not contain vector fields."
                    else
                        return $ WritableField (fieldName field) (fieldType field) []) fields
                protoCells <- either (clientError CEInvalidCSV . T.pack) return (parseProtoCSV csv)
                -- Save the CSV as a ProtoBlob.
                -- TODO: Add ad-hoc user datablock directory path to TrebEnv in Treb.Types and add a corresponding argument handler in Treb.Config.
                liftIO $ BL.writeFile ("user_datablocks/" ++ T.unpack name)  $
                    runPutBlob $
                        writeDB $
                            WritableDB name protoFields protoCells
                -- TODO: Write entry in PostgreSQL here.
                return $ DataBlockMetadataMsg 0 (AdHocName name (userName user)) fields (V.length csv))
            (decodeCSV defCSVSettings uploadContent)
    where
        parseProtoCSV :: V.Vector (V.Vector B.ByteString) -> Either String [ProtoCell]
        parseProtoCSV =
            V.foldM (\xs row ->
                fmap (++ xs) $ 
                    V.foldM
                        (\ys cell ->
                            either
                                Left
                                (maybe
                                    (Left "Failed to parse at least one CSV cell.")
                                    (Right . (:ys)))
                                $ A.parseOnly parseProtoCell cell) [] row) []
