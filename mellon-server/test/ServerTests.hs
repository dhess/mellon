{-# LANGUAGE OverloadedStrings #-}

module ServerTests (spec, sleep) where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Time.Clock
import Mellon.Server (State(..))
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Test.Hspec

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

serverState :: IO (Status, Maybe State)
serverState =
  do manager <- newManager defaultManagerSettings
     initialRequest <- parseUrl "http://localhost:8081/state"
     let request = initialRequest { method = "GET" }
     response <- httpLbs request manager
     return (responseStatus response, decode $ responseBody response)

serverTime :: IO (Status, Maybe UTCTime)
serverTime =
  do manager <- newManager defaultManagerSettings
     initialRequest <- parseUrl "http://localhost:8081/time"
     let request = initialRequest { method = "GET" }
     response <- httpLbs request manager
     return (responseStatus response, decode $ responseBody response)

lockIt :: IO (Status, Maybe State)
lockIt =
  do manager <- newManager defaultManagerSettings
     initialRequest <- parseUrl "http://localhost:8081/state"
     let request = initialRequest { method = "PUT"
                                  , requestBody = RequestBodyLBS $ encode Locked
                                  , requestHeaders = [("Content-Type", "application/json")] }
     response <- httpLbs request manager
     return (responseStatus response, decode $ responseBody response)

unlockIt :: UTCTime -> IO (Status, Maybe State)
unlockIt untilTime =
  do manager <- newManager defaultManagerSettings
     initialRequest <- parseUrl "http://localhost:8081/state"
     let request = initialRequest { method = "PUT"
                                  , requestBody = RequestBodyLBS $ encode (Unlocked untilTime)
                                  , requestHeaders = [("Content-Type", "application/json")] }
     response <- httpLbs request manager
     return (responseStatus response, decode $ responseBody response)

spec :: Spec
spec = do
  -- We assume these tests run in the specified order, as the same
  -- server is used for each test and the server obviously is
  -- stateful.

  describe "Initial server state" $ do
    it "should be locked" $ do
      serverState >>= (shouldBe (ok200, Just Locked))

  describe "Server time" $ do
    it "should be accurate" $ do
      now <- getCurrentTime
      (code, Just time) <- serverTime
      code `shouldBe` ok200
      let delta = 1.0 :: NominalDiffTime
      ((time `diffUTCTime` now) < delta) `shouldBe` True

  describe "Locking when locked" $ do
    it "is idempotent" $ do
      lockIt >>= shouldBe (ok200, Just Locked)
      serverState >>= shouldBe (ok200, Just Locked)

  describe "Unlock" $ do
    it "unlocks" $ do
      now <- getCurrentTime
      let untilTime = 3.0 `addUTCTime` now
      unlockIt untilTime >>= shouldBe (ok200, Just (Unlocked untilTime))
      serverState >>= shouldBe (ok200, Just (Unlocked untilTime))

    it "then locks when the unlock expires" $ do
      sleep 3
      serverState >>= shouldBe (ok200, Just Locked)

    it "overrides unlocks that expire earlier" $ do
      now <- getCurrentTime
      let untilTime = 3.0 `addUTCTime` now
      unlockIt untilTime >>= shouldBe (ok200, Just (Unlocked untilTime))
      sleep 1
      let newUntilTime = 7.0 `addUTCTime` now
      unlockIt newUntilTime >>= shouldBe (ok200, Just (Unlocked newUntilTime))
      serverState >>= shouldBe (ok200, Just (Unlocked newUntilTime))
      sleep 3
      serverState >>= shouldBe (ok200, Just (Unlocked newUntilTime))
      sleep 4
      serverState >>= shouldBe (ok200, Just Locked)

  describe "Lock" $ do
    it "overrides unlocks" $ do
      now <- getCurrentTime
      let untilTime = 20.0 `addUTCTime` now
      unlockIt untilTime >>= shouldBe (ok200, Just (Unlocked untilTime))
      serverState >>= shouldBe (ok200, Just (Unlocked untilTime))
      sleep 1
      lockIt >>= shouldBe (ok200, Just Locked)
      serverState >>= shouldBe (ok200, Just Locked)
