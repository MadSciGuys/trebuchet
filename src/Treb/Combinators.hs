module Treb.Combinators where

import Data.Bool
import Control.Exception
import Control.Monad.Trans.Either

leftIf :: Monad m => Bool -> l -> EitherT l m ()
leftIf b l = bool (return ()) (left l) b

catchAny :: (SomeException -> IO a) -> IO a -> IO a
catchAny = flip catch
