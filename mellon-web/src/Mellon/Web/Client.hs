{-|
Module      : Mellon.Web.Client
Description : Client actions for the Mellon REST web service
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Mellon.Web.Client
         ( -- * Re-exported for convenience.
           State(..)
         , Time(..)
           -- * Client functions
         , clientAPI
         , getTime
         , getState
         , putState
         ) where

import Control.Monad.Trans.Except (ExceptT)
import Data.Proxy
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client

import Mellon.Web.Server (MellonAPI, State(..), Time(..))

-- | The client API.
clientAPI :: Proxy MellonAPI
clientAPI = Proxy

-- | Generate the client functions for a given host and URL
-- scheme.
getTime :: Manager -> BaseUrl -> ExceptT ServantError IO Time
getState :: Manager -> BaseUrl -> ExceptT ServantError IO State
putState :: State -> Manager -> BaseUrl -> ExceptT ServantError IO State
getTime :<|> getState :<|> putState = client clientAPI
