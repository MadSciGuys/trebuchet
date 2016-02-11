{-|
Module:      Main
Description: Entry point module of the Trebuchet server.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}
module Main ( main ) where

import Treb.Config
import Treb.Env
import Treb.DB.Schema
import Treb.Routes.DataBlockCreate
import Treb.Routes.DataBlockFilter
import Treb.Routes.DataBlockGet
import Treb.Routes.DataBlockGetFilter
import Treb.Routes.DataBlockGetMetadata
import Treb.Routes.DataBlockPutMetadata
import Treb.Routes.DataBlockUpload
import Treb.Routes.FileUpload
import Treb.Routes.JobCreate
import Treb.Routes.JobFilter
import Treb.Routes.JobTemplateFilter
import Treb.Routes.Types
import Treb.Routes.UserFilter
import Treb.Routes.UserGet

-- | Entry point of Trebuchet server.
main :: IO ()
main = do
    cmd <- getTrebCmd
    case cmd of
        RunCmd config -> do
            init <- getTrebInit config
            case init of
                Left e ->
                    putStrLn $ "Trebuchet initialization failed.\n" ++ e
                Right _ ->
                    return ()
        DBCmd subcmd ->
            case subcmd of
                DBInitCmd config ->
                    initSchema config
                DBDropCmd config -> do
                    dropSchema config
