{-|
Module:      Treb.Routes.UserFilter
Description: Trebuchet UserFilter route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.UserFilter ( UserFilter, userFilter ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type UserFilter =
    -- TODO: Implement actual User filtering and then change this type to handle the /user/filter URL.
    "user" :> "all"
        :> Get '[JSON] [User]

userFilter :: TrebServer UserFilter
userFilter = undefined
