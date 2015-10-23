{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MellonServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (decode)
import Data.List
import Data.Time.Clock
import qualified Mellon.Controller as MC
import Mellon.Lock.Mock
import Mellon.Server (State(..), app, docsApp)
import Network.Wai
import Network.Wai.Handler.Warp
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

-- serverTime :: IO (Status, Maybe UTCTime)
-- serverTime =
--   do manager <- newManager defaultManagerSettings
--      initialRequest <- parseUrl "http://localhost:8081/time"
--      let request = initialRequest { method = "GET" }
--      response <- httpLbs request manager
--      return (responseStatus response, decode $ responseBody response)

-- lockIt :: IO (Status, Maybe State)
-- lockIt =
--   do manager <- newManager defaultManagerSettings
--      initialRequest <- parseUrl "http://localhost:8081/state"
--      let request = initialRequest { method = "PUT"
--                                   , requestBody = RequestBodyLBS $ encode Locked
--                                   , requestHeaders = [("Content-Type", "application/json")] }
--      response <- httpLbs request manager
--      return (responseStatus response, decode $ responseBody response)

-- unlockIt :: UTCTime -> IO (Status, Maybe State)
-- unlockIt untilTime =
--   do manager <- newManager defaultManagerSettings
--      initialRequest <- parseUrl "http://localhost:8081/state"
--      let request = initialRequest { method = "PUT"
--                                   , requestBody = RequestBodyLBS $ encode (Unlocked untilTime)
--                                   , requestHeaders = [("Content-Type", "application/json")] }
--      response <- httpLbs request manager
--      return (responseStatus response, decode $ responseBody response)

-- getDocs :: IO (Status, Maybe Header)
-- getDocs =
--   do manager <- newManager defaultManagerSettings
--      initialRequest <- parseUrl "http://localhost:8081/docs"
--      let request = initialRequest { method = "GET" }
--      response <- httpLbs request manager
--      let maybeCTHeader = find (\(h, _) -> h == hContentType) $ responseHeaders response
--      return (responseStatus response, maybeCTHeader)

-- -- Tests specific to the docsApp.
-- docsSpec :: Spec
-- docsSpec = do
--   describe "Docs" $ do
--     it "are available via GET /docs" $ do
--       getDocs >>= shouldBe (ok200, Just (hContentType, "text/plain"))

runApp :: MC.ConcurrentControllerCtx -> IO Application
runApp cc = return (app cc)

spec :: Spec
spec =
  do ml <- runIO $ mockLock
     cc <- runIO $ MC.concurrentControllerCtx ml
     with (runApp cc) $
       describe "Server" $
         do  it "should initially be locked" $
                get "/state" `shouldRespondWith` [json|{state:"Locked", until:[]}|]

  -- describe "Initial server state" $ do
  --   it "should be locked" $ do
  --     serverState >>= (shouldBe (ok200, Just Locked))

  -- describe "Server time" $ do
  --   it "should be accurate" $ do
  --     now <- getCurrentTime
  --     (code, Just time) <- serverTime
  --     code `shouldBe` ok200
  --     let delta = 1.0 :: NominalDiffTime
  --     ((time `diffUTCTime` now) < delta) `shouldBe` True

  -- describe "Locking when locked" $ do
  --   it "is idempotent" $ do
  --     lockIt >>= shouldBe (ok200, Just Locked)
  --     serverState >>= shouldBe (ok200, Just Locked)

  -- describe "Unlock" $ do
  --   it "unlocks" $ do
  --     now <- getCurrentTime
  --     let untilTime = 3.0 `addUTCTime` now
  --     unlockIt untilTime >>= shouldBe (ok200, Just (Unlocked untilTime))
  --     serverState >>= shouldBe (ok200, Just (Unlocked untilTime))

  --   it "then locks when the unlock expires" $ do
  --     sleep 3
  --     serverState >>= shouldBe (ok200, Just Locked)

  --   it "overrides unlocks that expire earlier" $ do
  --     now <- getCurrentTime
  --     let untilTime = 3.0 `addUTCTime` now
  --     unlockIt untilTime >>= shouldBe (ok200, Just (Unlocked untilTime))
  --     sleep 1
  --     let newUntilTime = 7.0 `addUTCTime` now
  --     unlockIt newUntilTime >>= shouldBe (ok200, Just (Unlocked newUntilTime))
  --     serverState >>= shouldBe (ok200, Just (Unlocked newUntilTime))
  --     sleep 3
  --     serverState >>= shouldBe (ok200, Just (Unlocked newUntilTime))
  --     sleep 4
  --     serverState >>= shouldBe (ok200, Just Locked)

  -- describe "Lock" $ do
  --   it "overrides unlocks" $ do
  --     now <- getCurrentTime
  --     let untilTime = 20.0 `addUTCTime` now
  --     unlockIt untilTime >>= shouldBe (ok200, Just (Unlocked untilTime))
  --     serverState >>= shouldBe (ok200, Just (Unlocked untilTime))
  --     sleep 1
  --     lockIt >>= shouldBe (ok200, Just Locked)
  --     serverState >>= shouldBe (ok200, Just Locked)
