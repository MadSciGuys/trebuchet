module Treb.Combinators where

import Data.Bool
import Control.Exception
import Control.Monad.Trans.Except

exceptIf :: Monad m => Bool -> e -> ExceptT e m ()
exceptIf b e = bool (return ()) (throwE e) b

catchAny :: (SomeException -> IO a) -> IO a -> IO a
catchAny = flip catch
