{-|
Module:      Treb.Routes.UserFilter
Description: Trebuchet UserFilter route type and function definitons.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Treb.Routes.UserFilter
    ( UserFilterH
    , userFilterH
    ) where

import Servant.API
import Treb.Routes.Types

---- Route-Specific Type ----
type UserFilterH =
    -- TODO: Implement actual User filtering and then change this type to handle the /user/filter URL.
    "user" :> "all"
        :> Get '[JSON] [User]

userFilterH :: TrebServer UserFilterH
userFilterH = undefined
