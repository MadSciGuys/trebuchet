{-|
Module:      Treb.Routes.JobFilter
Description: Trebuchet JobFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.JobFilter ( JobFilter, jobFilter ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type JobFilter =
    -- TODO

jobFilter :: TrebServer JobFilter
jobFilter = undefined
