{-|
Module:      Treb.Routes.UserGet
Description: Trebuchet UserGet route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.UserGet
    ( UserGetH
    , userGetH
    ) where

import Servant.API
import Treb.Routes.Types

---- Route-Specific Type ----
type UserGetH =
    "user" :> Capture "username" Text
        :> Get '[JSON] User

userGetH :: TrebServer UserGetH
userGetH = undefined
