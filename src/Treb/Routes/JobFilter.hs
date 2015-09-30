{-|
Module:      Treb.Routes.JobFilter
Description: Trebuchet JobFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.JobFilter ( JobFilterH, jobFilterH ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type JobFilterH =
    "job" :> "filter"
        :> ReqBody '[JSON] JobSetFilter
        :> Post '[JSON] Job

jobFilterH :: TrebServer JobFilterH
jobFilterH = undefined
