{-|
Module:      Treb.BadRegex
Description: Unfortunate Regex Evaluation on Data.Text
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitka@its.jnj.com
Stability:   Provisional
Portability: POSIX

Please fix this.
-}

module Treb.BadRegex where

import qualified Data.Text as T

import Text.Regex.TDFA

-- | Evaluate a regular expression over 'T.Text' by unpacking the 
badRegexBool :: T.Text -> T.Text -> Bool
badRegexBool x e = (T.unpack x) =~ (T.unpack e)
