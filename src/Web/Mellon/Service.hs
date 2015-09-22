{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.Mellon.Service
         ( app
         ) where

import Data.Aeson
import Data.Time
import GHC.Generics
import Network.Wai
import Servant

data State = Locked | Unlocked UTCTime deriving (Eq, Show, Generic)

instance ToJSON State

type StateAPI = "state" :> Get '[JSON] State

server :: Server StateAPI
server = return Locked

stateAPI :: Proxy StateAPI
stateAPI = Proxy

app :: Application
app = serve stateAPI server

