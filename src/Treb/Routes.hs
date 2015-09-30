{-|
Module:      Treb.Routes
Description: Trebuchet types.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes
    ( TrebApi
    , trebApiProxy
    , DataBlockCreateH
    , dataBlockCreateH ) where

import Servant

import Treb.Routes.DataBlockCreate      ( DataBlockCreateH, dataBlockCreateH )
--import Treb.Routes.DataBlockFilter      ( DataBlockFilterH )
--import Treb.Routes.DataBlockGet         ( DataBlockGetH )
--import Treb.Routes.DataBlockGetMetadata ( DataBlockGetMetadataH )
--import Treb.Routes.DataBlockGetFilter   ( DataBlockGetFilterH )
--import Treb.Routes.DataBlockPutMetadata ( DataBlockPutMetadataH )
--import Treb.Routes.JobCreate            ( JobCreateH )
--import Treb.Routes.JobFilter            ( JobFilterH )
--import Treb.Routes.UserFilter           ( UserFilterH )
--import Treb.Routes.UserGet              ( UserGetH )
--import Treb.Routes.JobTemplateFilter    ( JobTemplateFilterH )

---- Trebuchet API ----
type TrebApi =
    ---- DataBlock ----
    -- NOTE: The following requests captures the "datablock_id" in the URL. This is
    -- used instead of DataBlockName because serializing that cleanly is tricky.
    -- That ought to be done at a later point. This will require a query to
    -- PostgreSQL to determine the DataBlockName so that it may be looked up in the
    -- server DataBlockName to DataBlock mapping.
         DataBlockCreateH
--    :<|> DataBlockFilterH
--    :<|> DataBlockGetH
--    :<|> DataBlockGetMetadataH
--    :<|> DataBlockGetFilterH
--    :<|> DataBlockPutMetadataH
--
--    ---- Job ----
--    :<|> JobCreateH
--    :<|> JobFilterH
--
--    ---- User ----
--    :<|> UserFilterH
--    :<|> UserGetH
--
--    ---- Job Template ----
--    :<|> JobTemplateFilterH

trebApiProxy :: Proxy TrebApi
trebApiProxy = Proxy
