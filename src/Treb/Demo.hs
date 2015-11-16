{-# LANGUAGE CPP, DataKinds, DeriveGeneric, TypeFamilies, TypeOperators, OverloadedStrings #-}

module Treb.Demo where

import Data.Word

import qualified Data.Map as M

import Servant

import Treb.Types
import Treb.Filter
import Treb.JSON

type TrebAPI = "db" :> Get '[JSON] [DataBlock]
          :<|> "db" :> ReqBody '[JSON] (Filter (M.Map DataBlockName DataBlock)) :> Post '[JSON] [DataBlock]
          :<|> "q"  :> ReqBody '[JSON] Query :> Post '[JSON] Result
          :<|> "p"  :> Capture "pid" Word64 :> Get '[JSON] Page
