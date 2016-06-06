-- | A REST web service for interacting with @mellon-core@
-- controllers.

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

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (ExceptT)
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
import Mellon.Controller
       (Controller, lockController, unlockController, queryController)
import qualified Mellon.Controller as Controller (State(..))

wrapBody :: Monad m => HtmlT m () -> HtmlT m a -> HtmlT m a
wrapBody title body =
  doctypehtml_ $
    do head_ $
         title_ title
       body_ body

-- | Mimics 'Controller.State', but provides JSON conversions.
-- (Avoids orphan instances.)
data State = Locked | Unlocked UTCTime deriving (Eq, Show, Generic)

stateToState :: Controller.State -> State
stateToState Controller.StateLocked = Locked
stateToState (Controller.StateUnlocked date) = Unlocked date

stateJSONOptions :: Options
stateJSONOptions = defaultOptions {sumEncoding = taggedObject}
  where taggedObject =
          TaggedObject {tagFieldName = "state"
                       ,contentsFieldName = "until"}

instance ToJSON State where
  toJSON = genericToJSON stateJSONOptions

instance FromJSON State where
  parseJSON = genericParseJSON stateJSONOptions

sampleDate :: UTCTime
sampleDate = UTCTime { utctDay = fromGregorian 2015 10 06, utctDayTime = 0 }

instance ToSample State where
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

instance ToSample Time where
  toSamples _ = [("2015-10-06",Time sampleDate)]

timeDocument :: Monad m => HtmlT m a -> HtmlT m a
timeDocument = wrapBody "Server time"

instance ToHtml Time where
  toHtml (Time time) = timeDocument $ toHtml $ "Server time is " ++ show time
  toHtmlRaw = toHtml

-- | A "Servant" API for interacting with a @mellon-core@ controller.
-- The API also provides a way to obtain the system time on the
-- server, to ensure that the server's clock is accurate.
type MellonAPI =
  "time" :> Get '[JSON, HTML] Time :<|>
  "state" :> Get '[JSON, HTML] State :<|>
  "state" :> ReqBody '[JSON] State :> Put '[JSON, HTML] State

type AppM d m = ReaderT (Controller d) (ExceptT ServantErr m)

serverT :: (MonadIO m) => ServerT MellonAPI (AppM d m)
serverT =
  getTime :<|>
  getState :<|>
  putState
  where
    getTime :: (MonadIO m) => AppM d m Time
    getTime =
      do now <- liftIO getCurrentTime
         return $ Time now

    getState :: (MonadIO m) => AppM d m State
    getState =
      do cc <- ask
         fmap stateToState (queryController cc)

    putState :: (MonadIO m) => State -> AppM d m State
    putState Locked =
      do cc <- ask
         fmap stateToState (lockController cc)
    putState (Unlocked date) =
      do cc <- ask
         fmap stateToState (unlockController date cc)

-- | A 'Proxy' for 'MellonAPI', exported in order to make it possible
-- to extend the API.
mellonAPI :: Proxy MellonAPI
mellonAPI = Proxy

serverToEither :: (MonadIO m) => Controller d -> AppM d m :~> ExceptT ServantErr m
serverToEither cc = Nat $ \m -> runReaderT m cc

-- | A 'Server' which serves the 'MellonAPI' on the given
-- 'ControllerCtx' instance.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'MellonAPI'.
server :: Controller d -> Server MellonAPI
server cc = enter (serverToEither cc) serverT

-- | An 'Network.Wai.Application' which runs the server, using the given
-- 'ControllerCtx' instance for the controller.
app :: Controller d -> Application
app = serve mellonAPI . server
