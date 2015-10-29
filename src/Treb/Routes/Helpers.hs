{-|
Module:      Treb.Routes.Helpers
Description: Helper functions that return into TrebServerBase.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             LiberalTypeSynonyms, ImpredicativeTypes #-}
module Treb.Routes.Helpers where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.QueryParams as MySQL
import qualified Database.MySQL.Simple.QueryResults as MySQL
import qualified Hasql as H
import qualified Hasql.Postgres as HP
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar ()
import Control.Monad.Reader
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Either
import Data.Aeson
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import Network.URI
import Servant
import System.Random
import Treb.JSON ()
import Treb.Routes.Types
import Treb.Types
import Web.Cookie
import Control.Monad.Trans.Except

drupalAuth :: TrebServerBase a -> Maybe Text -> TrebServerBase a
drupalAuth action cookies = do
  let sessionCookie = fmap (fmap snd . find ((== "SESS249b7ba79335e5fe3b5934ff07174a20") . fst) . parseCookies . encodeUtf8) cookies
  conn <- fromJust <$> trebEnvDrupalMySQLConn <$> ask
  users <- maybe
    (lift $ throwE $ err403 { errBody = encode $ ClientError CEMissingSessionCookie "Drupal session cookie is not found." })
    (liftIO . MySQL.query conn "SELECT atrium_users.uid, atrium_users.name, atrium_realname.realname, atrium_users.mail FROM atrium_users INNER JOIN atrium_sessions ON atrium_users.uid = atrium_sessions.uid INNER JOIN atrium_realname ON atrium_users.uid = atrium_realname.uid WHERE atrium_sessions.sid = ?" . MySQL.Only)
    sessionCookie
  case users of
    [] ->
      lift $ throwE $ err403 { errBody = encode $ ClientError CEInvalidSessionCookie "Drupal session cookie was given but was not found in Drupal." }
    [(uid, username, realname, email)] ->
      local (setTrebEnvCurrentUser $ Just $ User uid username realname email) action
    _ ->
      lift $ throwE err500 { errBody = "SQL query invalid. Returned list of usernames has more than one element." }

getUserByUsername :: Text -> TrebServerBase User
getUserByUsername username = do
    rs <- queryDrupal (MySQL.Only username)
        "SELECT atrium_users.uid, atrium_users.name, atrium_realname.realname, atrium_users.mail FROM atrium_users INNER JOIN atrium_realname ON atrium_realname.uid = atrium_users.uid WHERE atrium_users.name = ?"
    case rs of
        [(drupalUid, drupalUsername, drupalRealname, drupalEmail)] ->
            return $ User drupalUid drupalUsername drupalRealname drupalEmail
        _ ->
            serverError "Drupal MySQL connection is Nothing in environment."
            

serverError :: B.ByteString -> TrebServerBase a
serverError msg = lift $ throwE $ err500 { errBody = msg }

clientError :: ClientErrorCode -> Text -> TrebServerBase a
clientError ce msg =
  (\ ret -> lift $ throwE $ ret { errBody = encode $ ClientError ce msg }) $ case ce of
    CEMissingSessionCookie -> err403
    CEInvalidSessionCookie -> err403
    CEUserNotFound         -> err404
    CEInvalidCSV           -> err400

queryDrupal :: (MySQL.QueryParams q, MySQL.QueryResults r) => q -> MySQL.Query -> TrebServerBase [r]
queryDrupal params query = do
    conn <- getDrupalMySQLConn
    liftIO $ MySQL.query conn query params

queryPG :: forall r. (forall s. H.Ex HP.Postgres s r) -> H.Stmt HP.Postgres -> TrebServerBase r
queryPG ex stmt = do
    pool <- getPgPool
    res <- liftIO $ H.session pool (H.tx Nothing (ex stmt :: H.Tx HP.Postgres s r) :: H.Session HP.Postgres IO r)
    either
        -- TODO: errBody = B.toStrict $ encodeUtf8 $ pack $ show err
        (lift . throwE . const err500 { errBody = "Postgres failure." })
        return
        res

freshUploadId :: TrebServerBase Int
freshUploadId = do
    user <- getCurrentUser
    idGen <- reader trebEnvUploadIdGen
    activeUploadsTVar <- reader trebEnvActiveUploads
    liftIO $ atomically $ readTVar activeUploadsTVar >>= findNewUploadId idGen user
    where
      findNewUploadId idGen user uploads = do
                  uploadId <- nextStdGenSTM idGen
                  if M.member (userName user, uploadId) uploads then
                      findNewUploadId idGen user uploads
                  else
                      return uploadId

nextStdGenSTM :: TVar StdGen -> STM Int
nextStdGenSTM idGen = do
  g <- readTVar idGen
  let (i, g') = next g
  writeTVar idGen g'
  return i

-- Environment Accessors --
getActiveUploads :: TrebServerBase ActiveUploads
getActiveUploads = reader trebEnvActiveUploads >>= liftIO . readTVarIO

getRandomUploadId :: TrebServerBase Int
getRandomUploadId = reader trebEnvUploadIdGen >>= liftIO . atomically . nextStdGenSTM

getCurrentUser :: TrebServerBase User
getCurrentUser = do
    maybeUser <- reader trebEnvCurrentUser
    maybe
        (serverError "Current user requested without authentication context.")
        return
        maybeUser

getBaseURI :: TrebServerBase URI
getBaseURI = reader trebEnvBaseURI

getDrupalMySQLConn :: TrebServerBase MySQL.Connection
getDrupalMySQLConn = do
    maybeConn <- reader trebEnvDrupalMySQLConn
    maybe 
      (lift $ throwE $ err500 { errBody = "Drupal MySQL connection is Nothing in environment." })
      return
      maybeConn

getPgPool :: TrebServerBase (H.Pool HP.Postgres)
getPgPool = reader trebEnvPgPool

-- TVar Modifiers --
modifyJobTemplates  :: ([JobTemplate] -> [JobTemplate]) -> TrebServerBase ()
modifyJobTemplates  = (reader trebEnvJobTemplates >>=) . tvarModify

modifyActiveUploads :: (ActiveUploads -> ActiveUploads) -> TrebServerBase ()
modifyActiveUploads = (reader trebEnvActiveUploads >>=) . tvarModify

modifyUploadIdGen   :: (StdGen -> StdGen) -> TrebServerBase ()
modifyUploadIdGen   = (reader trebEnvUploadIdGen >>=) . tvarModify

tvarModify :: (a -> a) -> TVar a -> TrebServerBase ()
tvarModify = (liftIO .) . (atomically .) . flip modifyTVar

-- createDataBlock :: DataBlockName -> [DataBlockField] -> TrebServer DataBlock
-- createDataBlock name fields =
