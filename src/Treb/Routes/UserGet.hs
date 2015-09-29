{-|
Module:      Treb.Routes.UserGet
Description: Trebuchet UserGet route type and function definitons.
Copyright:   Travis Whitaker 2015
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.UserGet ( UserGetH, userGet ) where

import Treb.Routes.Types

---- Route-Specific Type ----
type UserGetH =
    "user" :> Capture "username" Text
        :> Get '[JSON] User

userGet :: TrebServer UserGetH
userGet = undefined
