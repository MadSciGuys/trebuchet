{-|
Module:      Treb.Routes.JobCreate
Description: Trebuchet JobCreate route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.JobCreate ( JobCreate, jobCreate ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type JobCreate =
    "job" :> "create"
        :> ReqBody '[JSON] JobConfig
        :> DrupalAuth
        :> Post '[JSON] Job

jobCreate :: TrebServer JobCreate
jobCreate = undefined
