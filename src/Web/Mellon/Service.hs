{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Lucid
import Network.Wai
import Servant
import Servant.HTML.Lucid

wrapBody :: Monad m => HtmlT m () -> HtmlT m a -> HtmlT m a
wrapBody title body =
  doctypehtml_ $
    do head_ $
         do title_ title
       body_ body

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

stateDocument :: Monad m => HtmlT m a -> HtmlT m a
stateDocument = wrapBody "Mellon state"

instance ToHtml State where
  toHtml Locked = stateDocument "Locked"
  toHtml (Unlocked time) = stateDocument $ "Unlocked until " >> toHtml (show time)
  toHtmlRaw = toHtml

instance ToHtml UTCTime where
  toHtml t = toHtml (show t)
  toHtmlRaw = toHtml

type StateAPI =
  "time" :> Get '[JSON, HTML] UTCTime :<|>
  "state" :> Get '[JSON, HTML] State :<|>
  "command" :> ReqBody '[JSON] Command :> Post '[JSON, HTML] State

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
