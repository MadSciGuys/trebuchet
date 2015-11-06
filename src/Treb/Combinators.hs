{-|
Module:      Treb.Combinators
Description: Generic helper functions.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

module Treb.Combinators where

import Control.Monad.Except
import Control.Exception
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), (</>), bool)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC (unpack)
import qualified Hasql as H
import qualified Hasql.Postgres as HP

-- | Throw an exception in MonadError conditioned on some boolean expression.
throwIf :: MonadError e m =>
           Bool
        -> e
        -> m ()
throwIf True  = throwError
throwIf False = const $ return ()

-- | Catch all exceptions.
catchAll :: (SomeException -> IO a) -> IO a -> IO a
catchAll = flip catch

-- | Convert a Hasql PostgreSQL session error to a well-formated error message
--   string.
showSessionError :: H.SessionError HP.Postgres
                 -> String
showSessionError e =
    "PostgreSQL Error: " ++
    case e of
        H.CxError err ->
            case err of
                HP.CantConnect msg ->
                    "Connection Failure" ++ maybe "!" (": " ++) (BC.unpack <$> msg)
                HP.UnsupportedVersion v ->
                    "Unsupported Version: " ++ show v
        H.TxError err ->
            case err of
                HP.NoResult msg ->
                    "No Result" ++ maybe "!" (": " ++) (BC.unpack <$> msg)
                HP.ErroneousResult code primaryMsg secondaryMsg hint ->
                    "\n" ++
                    "    Erroneous Result:\n" ++
                    "        SQLSTATE: " ++ BC.unpack code ++ "\n" ++
                    "        Message:\n" ++ (show $ indent 12 $ text $ BC.unpack primaryMsg) ++
                    maybe ""
                          (("        Details:\n" ++) . show . indent 12 . text . BC.unpack)
                          secondaryMsg ++
                    maybe ""
                          (("        Hint:\n" ++) . show . indent 12 . text . BC.unpack)
                          hint
                HP.UnexpectedResult msg ->
                    "Unexpected Result: " ++ T.unpack msg
                HP.NotInTransaction ->
                    "Transactional query executed outside of transaction."
        H.ResultError msg ->
            "Result Error: " ++ T.unpack msg
