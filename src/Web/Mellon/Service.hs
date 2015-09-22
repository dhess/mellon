{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.Mellon.Service
         ( app
         ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import GHC.Generics
import Network.Wai
import Servant

data State = Locked | Unlocked UTCTime deriving (Eq, Show, Generic)

stateJSONOptions :: Options
stateJSONOptions = defaultOptions { sumEncoding = taggedObject }
  where
    taggedObject = defaultTaggedObject { tagFieldName = "state"
                                       , contentsFieldName = "until" }

instance ToJSON State where
  toJSON = genericToJSON stateJSONOptions

type StateAPI = "state" :> Get '[JSON] State

server :: Server StateAPI
server =
  do now <- liftIO $ getCurrentTime
     return $ Unlocked now

stateAPI :: Proxy StateAPI
stateAPI = Proxy

app :: Application
app = serve stateAPI server

