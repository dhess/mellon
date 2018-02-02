{-|
Module      : Mellon.Web.Server.API
Description : A REST web service for @mellon-core@ controllers
Copyright   : (c) 2018, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixoftic.com>
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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Lens ((&), (.~), (?~), mapped)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Types as Aeson (Value(String))
import Data.Aeson.Types
       ((.=), (.:), (.:?), FromJSON(..), Pair, Series, ToJSON(..),
        Value(Object), defaultOptions, genericToJSON, genericParseJSON,
        object, pairs, typeMismatch)
import Data.Data
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Swagger
       (NamedSchema(..), Referenced(Inline), SwaggerType(..),
        ToSchema(..), declareSchemaRef, defaultSchemaOptions, description,
        enum_, example, genericDeclareNamedSchema, properties, required,
        schema, type_)
import Data.Text (Text)
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
       ((:<|>)(..), (:>), (:~>)(..), Get, Handler, JSON, Proxy(..), Put,
        ReqBody, Server, ServerT, enter, serve)
import Servant.Docs (ToSample(..))
import Servant.HTML.Lucid (HTML)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson (eitherDecode, encode)
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

lockedName :: Text
lockedName = "Locked"

unlockedName :: Text
unlockedName = "Unlocked"

untilName :: Text
untilName = "until"

stateName :: Text
stateName = "state"

lockedPair :: Pair
lockedPair = stateName .= lockedName

unlockedPair :: Pair
unlockedPair = stateName .= unlockedName

untilPair :: UTCTime -> Pair
untilPair t = untilName .= t

lockedSeries :: Series
lockedSeries = stateName .= lockedName

unlockedSeries :: Series
unlockedSeries = stateName .= unlockedName

untilSeries :: UTCTime -> Series
untilSeries t = untilName .= t

-- $
-- >>> toJSON Locked
-- Object (fromList [("state",String "Locked")])
-- >>> toJSON $ Unlocked sampleDate
-- Object (fromList [("state",String "Unlocked"),("until",String "2015-10-06T00:00:00Z")])
-- >>> encode $ toJSON Locked
-- "{\"state\":\"Locked\"}"
-- >>> encode $ toJSON $ Unlocked sampleDate
-- "{\"state\":\"Unlocked\",\"until\":\"2015-10-06T00:00:00Z\"}"
instance ToJSON State where
  toJSON Locked = object [lockedPair]
  toJSON (Unlocked time) = object [unlockedPair, untilPair time]
  toEncoding Locked = pairs lockedSeries
  toEncoding (Unlocked time) = pairs $ unlockedSeries <> untilSeries time

-- $
-- >>> (eitherDecode $ encode $ toJSON $ Unlocked sampleDate) :: Either String State
-- Right (Unlocked 2015-10-06 00:00:00 UTC)
-- >>> eitherDecode $ "{\"state\":\"Unlocked\",\"until\":\"2017-05-05T22:30-08:00\"}" :: Either String State
-- Right (Unlocked 2017-05-06 06:30:00 UTC)
-- >>> (eitherDecode $ encode $ toJSON Locked) :: Either String State
-- Right Locked
-- >>> eitherDecode $ "{\"state\":\"Unlocked\"}" :: Either String State
-- Left "Error in $: 'Unlocked' state requires an expiration date"
-- >>> eitherDecode $ "{\"state\":\"Locked\",\"until\":\"2015-10-06T00:00:00Z\"}" :: Either String State
-- Left "Error in $: 'Locked' state takes no argument"
-- >>> eitherDecode $ "{\"state\":\"Unlocked\",\"until\":\"2015\"}" :: Either String State
-- Left "Error in $.until: could not parse date: '-': not enough input"
-- >>> eitherDecode $ "{\"state\":\"Lokced\"}" :: Either String State -- note: typo
-- Left "Error in $: Invalid 'state' value"
instance FromJSON State where
  parseJSON (Object v) = do
    state :: Text <- v .: stateName
    until_ :: Maybe Time <- v .:? untilName
    case state of
      "Locked" ->
        maybe
          (pure Locked)
          (const $ fail "'Locked' state takes no argument")
          until_
      "Unlocked" ->
        maybe
          (fail "'Unlocked' state requires an expiration date")
          (\(Time t) -> pure $ Unlocked t)
          until_
      _ -> fail "Invalid 'state' value"
  parseJSON invalid = typeMismatch "State" invalid

-- $
-- >>> validateToJSON Locked
-- []
-- >>> validateToJSON $ Unlocked sampleDate
-- []
instance ToSchema State where
  declareNamedSchema _ = do
    utcTimeSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
    let stateSchema =
          mempty & enum_ ?~ [Aeson.String lockedName, Aeson.String unlockedName]
                 & type_ .~ SwaggerString
    return $
      NamedSchema (Just "State") $
        mempty & type_ .~ SwaggerObject
               & properties .~ [(stateName, Inline stateSchema), (untilName, utcTimeSchema)]
               & required .~ [stateName]
               & description ?~ "The controller state; a variant type."
               & example ?~ toJSON (Unlocked sampleDate)

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

type AppM d = ReaderT (Controller d) Handler

serverT :: ServerT MellonAPI (AppM d)
serverT =
  getTime :<|>
  getState :<|>
  putState
  where
    getTime :: AppM d Time
    getTime = Time <$> liftIO getCurrentTime

    getState :: AppM d State
    getState =
      do cc <- ask
         fmap stateToState (queryController cc)

    putState :: State -> AppM d State
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

appToHandler :: Controller d -> AppM d :~> Handler
appToHandler cc = NT $ \m -> runReaderT m cc

-- | A Servant 'Server' which serves the 'MellonAPI' on the given
-- 'Controller'.
--
-- Normally you will just use 'app', but this function is exported so
-- that you can extend/wrap 'MellonAPI'.
server :: Controller d -> Server MellonAPI
server cc = enter (appToHandler cc) serverT

-- | A WAI 'Network.Wai.Application' which runs the service, using the
-- given 'Controller'.
app :: Controller d -> Application
app = serve mellonAPI . server
