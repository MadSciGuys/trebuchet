{-|
Module:      Treb.Routes.JobCreate
Description: Trebuchet JobCreate route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.JobCreate
    ( JobCreateH
    , jobCreateH
    ) where

import Servant.API
import Treb.Routes.Types

---- Route-Specific Type ----
type JobCreateH =
    "job" :> "create"
        :> ReqBody '[JSON] JobConfig
        :> DrupalAuth
        :> Post '[JSON] Job

jobCreateH :: TrebServer JobCreateH
jobCreateH = undefined
