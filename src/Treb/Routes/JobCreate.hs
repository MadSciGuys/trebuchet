{-|
Module:      Treb.Routes.JobCreate
Description: Trebuchet JobCreate route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.JobCreate ( JobCreateH, jobCreate ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type JobCreateH =
    "job" :> "create"
        :> ReqBody '[JSON] JobConfig
        :> DrupalAuth
        :> Post '[JSON] Job

jobCreate :: TrebServer JobCreateH
jobCreate = undefined
