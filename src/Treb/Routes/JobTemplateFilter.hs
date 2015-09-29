{-|
Module:      Treb.Routes.JobTemplateFilter
Description: Trebuchet JobTemplateFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.JobTemplateFilter ( JobTemplateFilterH, jobTemplateFilter ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type JobTemplateFilterH =
    -- TODO: Implement actual JobTemplate filtering and then change this type to handle the /job_template/filter URL.
    "job_template" :> "all"
        :> Get '[JSON] [JobTemplate]

jobTemplateFilter :: TrebServer JobTemplateFilterH
jobTemplateFilter = undefined
