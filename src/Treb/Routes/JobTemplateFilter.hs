{-|
Module:      Treb.Routes.JobTemplateFilter
Description: Trebuchet JobTemplateFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.JobTemplateFilter ( JobTemplateFilter, jobTemplateFilter ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type JobTemplateFilter =
    -- TODO: Implement actual JobTemplate filtering and then change this type to handle the /job_template/filter URL.
    "job_template" :> "all"
        :> Get '[JSON] [JobTemplate]

jobTemplateFilter :: TrebServer JobTemplateFilter
jobTemplateFilter = undefined
