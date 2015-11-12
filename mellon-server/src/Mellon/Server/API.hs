-- | A REST web service for interacting with @mellon@ controllers.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Mellon.Server.API
         ( MellonAPI
         , State(..)
         , Time(..)
         , app
         , mellonAPI
         , server
         ) where

import Control.Monad.Trans.Either (EitherT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics
import Lucid
import Network.Wai
import Servant
import Servant.Docs
import Servant.HTML.Lucid
import Mellon.Monad.Controller (ControllerCtx, ControllerT, MonadController(..), runControllerT)
import qualified Mellon.Monad.Controller as Controller (State(..))

wrapBody :: Monad m => HtmlT m () -> HtmlT m a -> HtmlT m a
wrapBody title body =
  doctypehtml_ $
    do head_ $
         do title_ title
       body_ body

-- | Mimics 'Controller.State', but provides JSON conversions.
-- (Avoids orphan instances.)
data State = Locked | Unlocked UTCTime deriving (Eq, Show, Generic)

stateToState :: Controller.State -> State
stateToState Controller.Locked = Locked
stateToState (Controller.Unlocked date) = Unlocked date

stateJSONOptions :: Options
stateJSONOptions = defaultOptions { sumEncoding = taggedObject }
  where
    taggedObject = defaultTaggedObject { tagFieldName = "state"
                                       , contentsFieldName = "until" }

instance ToJSON State where
  toJSON = genericToJSON stateJSONOptions

instance FromJSON State where
  parseJSON = genericParseJSON stateJSONOptions

sampleDate :: UTCTime
sampleDate = UTCTime { utctDay = fromGregorian 2015 10 06, utctDayTime = 0 }

instance ToSample State State where
  toSamples _ =
    [ ("Locked", Locked)
    , ("Unlocked until a given date", Unlocked sampleDate)
    ]

stateDocument :: Monad m => HtmlT m a -> HtmlT m a
stateDocument = wrapBody "Mellon state"

instance ToHtml State where
  toHtml Locked = stateDocument "Locked"
  toHtml (Unlocked time) = stateDocument $ "Unlocked until " >> toHtml (show time)
  toHtmlRaw = toHtml

-- | A newtype wrapper around 'UTCTime', for serving HTML without
-- orphan instances.
newtype Time = Time UTCTime deriving (Eq, Show, Generic)

instance ToJSON Time where
  toJSON = genericToJSON defaultOptions

instance FromJSON Time where
  parseJSON = genericParseJSON defaultOptions

instance ToSample Time Time where
  toSample _ = Just $ Time sampleDate

timeDocument :: Monad m => HtmlT m a -> HtmlT m a
timeDocument = wrapBody "Server time"

instance ToHtml Time where
  toHtml (Time time) = timeDocument $ toHtml $ "Server time is " ++ (show time)
  toHtmlRaw = toHtml

-- | A "Servant" API for interacting with a @mellon@ controller. The API also
-- provides a way to obtain the system time on the server, to ensure
-- that the server's clock is accurate.
type MellonAPI =
  "time" :> Get '[JSON, HTML] Time :<|>
  "state" :> Get '[JSON, HTML] State :<|>
  "state" :> ReqBody '[JSON] State :> Put '[JSON, HTML] State

type AppM m = ControllerT (EitherT ServantErr m)

serverT :: (MonadIO m) => ServerT MellonAPI (AppM m)
serverT =
  getTime :<|>
  getState :<|>
  putState
  where
    getTime :: (MonadIO m) => AppM m Time
    getTime =
      do now <- liftIO $ getCurrentTime
         return $ Time now

    getState :: (MonadIO m) => AppM m State
    getState = state >>= return . stateToState

    putState :: (MonadIO m) => State -> AppM m State
    putState Locked = lockNow >>= return . stateToState
    putState (Unlocked date) = unlockUntil date >>= return . stateToState

-- | A 'Proxy' for 'MellonAPI', exported in order to make it possible
-- to extend the API.
mellonAPI :: Proxy MellonAPI
mellonAPI = Proxy

serverToEither :: (MonadIO m) => ControllerCtx -> AppM m :~> EitherT ServantErr m
serverToEither cc = Nat $ \m -> runControllerT cc m

-- | A 'Server' which serves the 'MellonAPI' on the given
-- 'ControllerCtx' instance.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'MellonAPI'.
server :: ControllerCtx -> Server MellonAPI
server cc = enter (serverToEither cc) serverT

-- | An 'Network.Wai.Application' which runs the server, using the given
-- 'ControllerCtx' instance for the controller.
app :: ControllerCtx -> Application
app = serve mellonAPI . server
