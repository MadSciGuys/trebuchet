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
             QuasiQuotes, LiberalTypeSynonyms, ImpredicativeTypes #-}
module Treb.Routes.Helpers where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Database.MySQL.Simple as MySQL
import qualified Database.MySQL.Simple.QueryParams as MySQL
import qualified Database.MySQL.Simple.QueryResults as MySQL
import qualified Hasql as H
import qualified Hasql.Postgres as HP
import Data.Word
import Data.Aeson
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Data.Maybe
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Data.Text (Text)
import Servant
import Data.List (find)
import Web.Cookie
import Data.Text.Encoding
import Data.Bool
import Treb.Config
import Treb.Types
import Treb.Routes.Types
import Data.Time.Clock
import Data.Functor.Identity
import Control.Concurrent.STM
import System.Random

drupalAuth :: (User -> TrebServerBase a) -> Maybe Text -> TrebServerBase a
drupalAuth action cookies = do
  let sessionCookie = fmap (fmap snd . find ((== "SESS249b7ba79335e5fe3b5934ff07174a20") . fst) . parseCookies . encodeUtf8) cookies
  conn <- fromJust <$> trebEnvDrupalMySQLConn <$> ask
  users <- maybe
    (lift $ left $ err403 { errBody = encode $ ClientError CEMissingSessionCookie "Drupal session cookie is not found." })
    (liftIO . MySQL.query conn "SELECT atrium_users.uid, atrium_users.name, atrium_realname.realname, atrium_users.mail FROM atrium_users INNER JOIN atrium_sessions ON atrium_users.uid = atrium_sessions.uid INNER JOIN atrium_realname ON atrium_users.uid = atrium_realname.uid WHERE atrium_sessions.sid = ?" . MySQL.Only)
    sessionCookie
  case users of
    [] ->
      lift $ left $ err403 { errBody = encode $ ClientError CEInvalidSessionCookie "Drupal session cookie was given but was not found in Drupal." }
    [(uid, username, realname, email)] -> do
      ret <- action $ User uid username realname email
      return ret
    _ ->
      lift $ left err500 { errBody = "SQL query invalid. Returned list of usernames has more than one element." }

getUser :: Text -> TrebServerBase User
getUser username = do
    rs <- queryDrupal (MySQL.Only username)
        "SELECT atrium_users.uid, atrium_users.name, atrium_realname.realname, atrium_users.mail FROM atrium_users INNER JOIN atrium_realname ON atrium_realname.uid = atrium_users.uid WHERE atrium_users.name = ?"
    case rs of
        [(drupalUid, drupalUsername, drupalRealname, drupalEmail)] ->
            return $ User drupalUid drupalUsername drupalRealname drupalEmail
        _ ->
            serverError "Drupal MySQL connection is Nothing in environment."
            

serverError :: B.ByteString -> TrebServerBase a
serverError msg = lift $ left $ err500 { errBody = msg }

clientError :: ClientErrorCode -> Text -> TrebServerBase a
clientError ce msg =
  (\ ret -> lift $ left $ ret { errBody = encode $ ClientError ce msg }) $ case ce of
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
        (lift . left . const err500 { errBody = "Postgres failure." }) -- TODO: errBody = B.toStrict $ encodeUtf8 $ pack $ show err
        return
        res

getDrupalMySQLConn :: TrebServerBase MySQL.Connection
getDrupalMySQLConn = do
    maybeConn <- reader trebEnvDrupalMySQLConn
    maybe 
      (lift $ left $ err500 { errBody = "Drupal MySQL connection is Nothing in environment." })
      return
      maybeConn

getPgPool :: TrebServerBase (H.Pool HP.Postgres)
getPgPool = reader trebEnvPgPool

fileUpload :: forall a. TrebServer (FileUploadH a) -> TrebServerBase URI
fileUpload handler = do
    uploadId <- freshUploadId
    let p = Proxy :: Proxy (FileUploadH a)
    return $ safeLink p p uploadId

freshUploadId :: TrebServerBase Int
freshUploadId = do
    uploadId <- getRandomUploadId
    uploads <- getActiveUploads

    if M.member uploadId uploads then
        freshUploadId
    else
        return uploadId

getActiveUploads :: TrebServerBase (M.Map Int (TrebServer (forall a. FileUploadH a)))
getActiveUploads = reader trebEnvActiveUploads >>= liftIO . readTVarIO

getRandomUploadId :: TrebServerBase Int
getRandomUploadId = do
    idGen <- reader trebEnvUploadIdGen
    liftIO $ atomically $ do
        g <- readTVar idGen
        let (i, g') = next g
        writeTVar idGen g'
        return i
