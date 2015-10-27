{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module MellonServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)
import Data.List
import Data.Time.Clock
import qualified Mellon.Controller as MC
import Mellon.Lock.Mock
import Mellon.Server (State(..), app, docsApp)
import Network.HTTP.Types (hContentType, methodPut)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Test (SResponse, simpleBody)
import Test.Hspec
import Test.Hspec.Wai (WaiSession, get, liftIO, request, shouldRespondWith, with)

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

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

runApp :: IO Application
runApp =
  do ml <- mockLock
     cc <- MC.concurrentControllerCtx ml
     return (app cc)

putJSON :: ByteString -> LB.ByteString -> WaiSession SResponse
putJSON path = request methodPut path [(hContentType, "application/json;charset=utf-8")]

spec :: Spec
spec =
  do describe "Server time" $
       do with runApp $
            do it "should be accurate" $
                 do now <- liftIO getCurrentTime
                    response <- get "/time"
                    let Just (serverTime :: UTCTime) = decode $ simpleBody response
                    let delta = 1.0 :: NominalDiffTime
                    liftIO $ ((serverTime `diffUTCTime` now < delta)) `shouldBe` True

     describe "Initial server state" $
       do ml <- runIO $ mockLock
          cc <- runIO $ MC.concurrentControllerCtx ml
          now <- runIO $ getCurrentTime
          let untilTime = 30.0 `addUTCTime` now
          with (return $ app cc) $
            do it "should reflect the controller state" $
                 do response <- get "/state"
                    liftIO $ decode (simpleBody response) `shouldBe` Just Locked
                    putJSON "/state" (encode $ Unlocked untilTime) `shouldRespondWith` 200
          with (return $ app cc) $
            do it "even when reusing the same controller in a new server" $
                 do response <- get "/state"
                    liftIO $ decode (simpleBody response) `shouldBe` Just (Unlocked untilTime)

     describe "Locking when locked" $
       do with runApp $
            do it "should be idempotent" $
                 do response <- putJSON "/state" (encode Locked)
                    liftIO $ decode (simpleBody response) `shouldBe` Just Locked

     describe "PUT /state" $
       do with runApp $
            do it "should return the new state when locked->unlocked" $
                 do now <- liftIO getCurrentTime
                    let untilTime = 10.0 `addUTCTime` now
                    response <- putJSON "/state" (encode $ Unlocked untilTime)
                    liftIO $ decode (simpleBody response) `shouldBe` Just (Unlocked untilTime)
                    response <- get "/state"
                    liftIO $ decode (simpleBody response) `shouldBe` Just (Unlocked untilTime)
               it "should return the new state when unlocked->locked" $
                 do response <- putJSON "/state" (encode Locked)
                    liftIO $ decode (simpleBody response) `shouldBe` Just Locked
                    response <- get "/state"
                    liftIO $ decode (simpleBody response) `shouldBe` Just Locked

     describe "Unlocking" $
       do with runApp $
            do it "expires at the specified date" $
                 do now <- liftIO getCurrentTime
                    let untilTime = 5.0 `addUTCTime` now
                    response <- putJSON "/state" (encode $ Unlocked untilTime)
                    liftIO $ decode (simpleBody response) `shouldBe` Just (Unlocked untilTime)
                    liftIO $ sleep 2
                    response <- get "/state"
                    liftIO $ decode (simpleBody response) `shouldBe` Just (Unlocked untilTime)
                    liftIO $ sleep 3
                    response <- get "/state"
                    liftIO $ decode (simpleBody response) `shouldBe` Just Locked
          with runApp $
            do it "overrides current unlocks that expire earlier" $
                 do now <- liftIO getCurrentTime
                    let untilTime = 3.0 `addUTCTime` now
                    _ <- putJSON "/state" (encode $ Unlocked untilTime)
                    liftIO $ sleep 1
                    let laterUntilTime = 7.0 `addUTCTime` now
                    response <- putJSON "/state" (encode $ Unlocked laterUntilTime)
                    liftIO $ decode (simpleBody response) `shouldBe` Just (Unlocked laterUntilTime)
                    liftIO $ sleep 9
                    response <- get "/state"
                    liftIO $ decode (simpleBody response) `shouldBe` Just Locked

     describe "Locking" $
       do with runApp $
            do it "overrides unlocks" $
                 do now <- liftIO $ getCurrentTime
                    let untilTime = 20.0 `addUTCTime` now
                    _ <- putJSON "/state" (encode $ Unlocked untilTime)
                    liftIO $ sleep 1
                    response <- putJSON "/state" (encode Locked)
                    liftIO $ decode (simpleBody response) `shouldBe` Just Locked
