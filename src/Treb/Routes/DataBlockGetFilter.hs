{-|
Module:      Treb.Routes.DataBlockGetFilter
Description: Trebuchet DataBlockGetFilter route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.DataBlockGetFilter
    ( DataBlockGetFilterH
    , dataBlockGetFilterH
    ) where

import Servant.API
import Treb.Routes.Types
import qualified Data.Text as T

---- Route-Specific Type ----
type DataBlockGetFilterH =
    "datablock" :> Capture "datablock_id" DataBlockId :> "filter"
        :> ReqBody '[JSON] DataBlockGetFilterMsg
        :> Post '[JSON] DataBlockRecords

dataBlockGetFilterH :: TrebServer DataBlockGetFilterH
dataBlockGetFilterH dbId msg = do
    db <- getDataBlock dbId
    if isWhiteList msg then
        columns msg
    columns msg
