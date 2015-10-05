{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Mellon.Server
         ( Command(..)
         , State(..)
         , app
         ) where

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
import qualified Mellon.Controller as MC

wrapBody :: Monad m => HtmlT m () -> HtmlT m a -> HtmlT m a
wrapBody title body =
  doctypehtml_ $
    do head_ $
         do title_ title
       body_ body

data Command = LockNow | UnlockUntil !UTCTime deriving (Eq, Show, Generic)

commandJSONOptions :: Options
commandJSONOptions = defaultOptions { sumEncoding = taggedObject }
  where
    taggedObject = defaultTaggedObject { tagFieldName = "command"
                                       , contentsFieldName = "until" }

instance FromJSON Command where
  parseJSON = genericParseJSON commandJSONOptions

-- | Mimics 'Mellon.Controller.State', but provides JSON conversions.
-- (Avoids orphan instances.)
data State = Locked | Unlocked UTCTime deriving (Eq, Show, Generic)

stateToState :: MC.State -> State
stateToState MC.Locked = Locked
stateToState (MC.Unlocked date) = Unlocked date

stateJSONOptions :: Options
stateJSONOptions = defaultOptions { sumEncoding = taggedObject }
  where
    taggedObject = defaultTaggedObject { tagFieldName = "state"
                                       , contentsFieldName = "until" }

instance ToJSON State where
  toJSON = genericToJSON stateJSONOptions

instance FromJSON State where
  parseJSON= genericParseJSON stateJSONOptions

stateDocument :: Monad m => HtmlT m a -> HtmlT m a
stateDocument = wrapBody "Mellon state"

instance ToHtml State where
  toHtml Locked = stateDocument "Locked"
  toHtml (Unlocked time) = stateDocument $ "Unlocked until " >> toHtml (show time)
  toHtmlRaw = toHtml

newtype Time = Time UTCTime deriving (Eq, Show, Generic)

instance ToJSON Time where
  toJSON = genericToJSON defaultOptions

timeDocument :: Monad m => HtmlT m a -> HtmlT m a
timeDocument = wrapBody "Server time"

instance ToHtml Time where
  toHtml (Time time) = timeDocument $ toHtml $ "Server time is " ++ (show time)
  toHtmlRaw = toHtml

type MellonAPI =
  "time" :> Get '[JSON, HTML] Time :<|>
  "state" :> Get '[JSON, HTML] State :<|>
  "command" :> ReqBody '[JSON] Command :> Post '[JSON, HTML] State

type AppM = MC.ConcurrentControllerT (EitherT ServantErr IO)

serverT :: ServerT MellonAPI AppM
serverT =
  getTime :<|>
  getState :<|>
  execCommand
  where
    getTime :: AppM Time
    getTime =
      do now <- liftIO $ getCurrentTime
         return $ Time now

    getState :: AppM State
    getState = MC.state >>= return . stateToState

    execCommand :: Command -> AppM State
    execCommand LockNow = MC.lockNow >>= return . stateToState

    execCommand (UnlockUntil date) = MC.unlockUntil date >>= return . stateToState

mellonAPI :: Proxy MellonAPI
mellonAPI = Proxy

serverToEither :: MC.ConcurrentControllerCtx -> AppM :~> EitherT ServantErr IO
serverToEither cc = Nat $ \m -> MC.runConcurrentControllerT cc m

adaptServer :: MC.ConcurrentControllerCtx -> Server MellonAPI
adaptServer cc = enter (serverToEither cc) serverT

app :: MC.ConcurrentControllerCtx -> Application
app = serve mellonAPI . adaptServer
