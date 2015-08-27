module ExampleGen (main, p, module Treb.ExtTypes, module Data.Aeson) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Text.Encoding
import Treb.ExtTypes
import Treb.JSON

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Treb.Test.Gen
import Treb.ExtTypes
import Treb.ExtJSON
import Control.Monad
import Data.Monoid
import System.Directory

main = do
  examples <- sample' (arbitrary :: Gen DataBlockSource)
  let example = head examples
  createDirectory "out"
  forM (zip examples [1..]) $ \ (db, i) -> do
    let content = T.unpack $ decodeUtf8 $ B.toStrict $ encodePretty db
    writeFile ("out/test_" <> show i <> ".txt") content

p :: ToJSON a => a -> String
p = T.unpack . decodeUtf8 . B.toStrict . encodePretty
