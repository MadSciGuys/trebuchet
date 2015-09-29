{-|
Module:      Treb.Routes.UserGet
Description: Trebuchet UserGet route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators,
             ScopedTypeVariables, OverloadedStrings, FlexibleContexts,
             QuasiQuotes #-}

module Treb.Routes.UserGet ( UserGet, userGet ) where

import Servant
import Treb.Routes.Types

---- Route-Specific Type ----
type UserGet =
    "user" :> Capture "username" Text
        :> Get '[JSON] User

userGet :: TrebServer UserGet
userGet = undefined
