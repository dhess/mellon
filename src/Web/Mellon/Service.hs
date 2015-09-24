{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.Mellon.Service
         ( State(..)
         , app
         ) where

import Control.Concurrent.MVar (MVar, readMVar, swapMVar)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import GHC.Generics
import Network.Wai
import Servant

data Command = LockNow | UnlockUntil UTCTime deriving (Eq, Show, Generic)

commandJSONOptions :: Options
commandJSONOptions = defaultOptions { sumEncoding = taggedObject }
  where
    taggedObject = defaultTaggedObject { tagFieldName = "command"
                                       , contentsFieldName = "until" }

instance FromJSON Command where
  parseJSON = genericParseJSON commandJSONOptions

data State = Locked | Unlocked UTCTime deriving (Eq, Show, Generic)

stateJSONOptions :: Options
stateJSONOptions = defaultOptions { sumEncoding = taggedObject }
  where
    taggedObject = defaultTaggedObject { tagFieldName = "state"
                                       , contentsFieldName = "until" }

instance ToJSON State where
  toJSON = genericToJSON stateJSONOptions

type StateAPI =
  "time" :> Get '[JSON] UTCTime :<|>
  "state" :> Get '[JSON] State :<|>
  "command" :> ReqBody '[JSON] Command :> Post '[JSON] State

server :: MVar State -> Server StateAPI
server m =
  time :<|>
  state :<|>
  command
  where
    time :: EitherT ServantErr IO UTCTime
    time =
      do now <- liftIO $ getCurrentTime
         return now

    state :: EitherT ServantErr IO State
    state =
      do s <- liftIO $ readMVar m
         return s

    command :: Command -> EitherT ServantErr IO State
    command LockNow =
      do _ <- liftIO $ swapMVar m Locked
         return Locked
    command (UnlockUntil date) =
      do _ <- liftIO $ swapMVar m (Unlocked date)
         return $ Unlocked date

stateAPI :: Proxy StateAPI
stateAPI = Proxy

app :: MVar State -> Application
app = serve stateAPI . server
