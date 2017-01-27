{-|
Module      : Mellon.Web.Server.API
Description : A REST web service for @mellon-core@ controllers
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

This module provides a "Servant" REST web service for @mellon-core@
controllers. The service translates REST methods to controller
actions.

See the included <API.md API.md> file for detailed documentation on
the REST service methods and document types.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}

module Mellon.Web.Server.API
         ( -- * Types
           MellonAPI
         , State(..)
         , Time(..)

           -- * Servant / WAI functions
         , app
         , mellonAPI
         , server
         ) where

import Control.Lens ((&), (?~), mapped)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Types
       (FromJSON(..), ToJSON(..), Options(..), SumEncoding(TaggedObject),
        defaultOptions, genericToJSON, genericParseJSON, tagFieldName,
        contentsFieldName)
import Data.Data
import Data.Swagger
       (ToSchema(..), defaultSchemaOptions, description, example,
        genericDeclareNamedSchema, schema)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import GHC.Generics
import Lucid
       (ToHtml(..), HtmlT, doctypehtml_, head_, title_, body_)
import Mellon.Controller
       (Controller, lockController, unlockController, queryController)
import qualified Mellon.Controller as Controller (State(..))
import Network.Wai (Application)
import Servant
       ((:>), (:<|>)(..), (:~>)(..), JSON, Get, ReqBody, Put, Proxy(..),
        ServerT, Server, ServantErr, enter, serve)
import Servant.Docs (ToSample(..))
import Servant.HTML.Lucid (HTML)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Swagger.Schema.Validation

wrapBody :: Monad m => HtmlT m () -> HtmlT m a -> HtmlT m a
wrapBody title body =
  doctypehtml_ $
    do head_ $
         title_ title
       body_ body

-- | Mimics 'Controller.State', but provides JSON conversions.
-- (Avoids orphan instances.)
data State
  = Locked
  | Unlocked !UTCTime
  deriving (Eq, Data, Read, Show, Generic, Typeable)

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

-- $
-- >>> validateToJSON Locked
-- []
-- >>> validateToJSON $ Unlocked sampleDate
-- []
instance ToSchema State where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped.schema.description ?~ "The controller state"
      & mapped.schema.example ?~ toJSON (Unlocked sampleDate)

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
newtype Time =
  Time UTCTime
  deriving (Eq, Data, Ord, Read, Show, Generic, Typeable)

instance ToJSON Time where
  toJSON = genericToJSON defaultOptions

instance FromJSON Time where
  parseJSON = genericParseJSON defaultOptions

-- $
-- >>> validateToJSON sampleDate
-- []
instance ToSchema Time where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped.schema.description ?~ "A UTC date"
      & mapped.schema.example ?~ toJSON sampleDate

instance ToSample Time where
  toSamples _ = [("2015-10-06",Time sampleDate)]

timeDocument :: Monad m => HtmlT m a -> HtmlT m a
timeDocument = wrapBody "Server time"

instance ToHtml Time where
  toHtml (Time time) = timeDocument $ toHtml $ "Server time is " ++ show time
  toHtmlRaw = toHtml

-- | A "Servant" API for interacting with a 'Controller'.
--
-- In addition to the controller methods, the API also provides a way
-- to obtain the system time on the server, to ensure that the
-- server's clock is accurate.
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

-- | A Servant 'Server' which serves the 'MellonAPI' on the given
-- 'Controller'.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'MellonAPI'.
server :: Controller d -> Server MellonAPI
server cc = enter (serverToEither cc) serverT

-- | A WAI 'Network.Wai.Application' which runs the service, using the
-- given 'Controller'.
app :: Controller d -> Application
app = serve mellonAPI . server
