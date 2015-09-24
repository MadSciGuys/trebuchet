module Treb.Combinators where

import Data.Bool
import Control.Monad.Trans.Either

leftIf :: Monad m => Bool -> l -> EitherT l m ()
leftIf b l = bool (return ()) (left l) b
