{-|
Module:      Treb.Routes.JobTemplateFilter
Description: Trebuchet JobTemplateFilter route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.JobTemplateFilter
    ( JobTemplateFilterH
    , jobTemplateFilterH
    ) where

import Servant.API
import Treb.Routes.Types

---- Route-Specific Type ----
type JobTemplateFilterH =
    "job_template" :> "filter"
        :> Get '[JSON] [JobTemplate]

jobTemplateFilterH :: TrebServer JobTemplateFilterH
jobTemplateFilterH = undefined
