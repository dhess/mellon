{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MellonServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)
import Data.Time.Clock
import Mellon.Monad.Controller (controllerCtx)
import Mellon.Device.MockLock
import Mellon.Server (State(..), app, docsApp)
import Network.HTTP.Types (hContentType, methodPut)
import Network.Wai
import Network.Wai.Test (SResponse, simpleBody)
import Test.Hspec
import Test.Hspec.Wai ((<:>), WaiSession, get, liftIO, matchHeaders, request, shouldRespondWith, with)

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

runApp :: IO Application
runApp =
  do ml <- mockLock
     cc <- controllerCtx ml
     return (app cc)

runDocsApp :: IO Application
runDocsApp =
  do ml <- mockLock
     cc <- controllerCtx ml
     return (docsApp cc)

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
          cc <- runIO $ controllerCtx ml
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
                    firstResponse <- putJSON "/state" (encode $ Unlocked untilTime)
                    liftIO $ decode (simpleBody firstResponse) `shouldBe` Just (Unlocked untilTime)
                    secondResponse <- get "/state"
                    liftIO $ decode (simpleBody secondResponse) `shouldBe` Just (Unlocked untilTime)
               it "should return the new state when unlocked->locked" $
                 do firstResponse <- putJSON "/state" (encode Locked)
                    liftIO $ decode (simpleBody firstResponse) `shouldBe` Just Locked
                    secondResponse <- get "/state"
                    liftIO $ decode (simpleBody secondResponse) `shouldBe` Just Locked

     describe "Unlocking" $
       do with runApp $
            do it "expires at the specified date" $
                 do now <- liftIO getCurrentTime
                    let untilTime = 5.0 `addUTCTime` now
                    firstResponse <- putJSON "/state" (encode $ Unlocked untilTime)
                    liftIO $ decode (simpleBody firstResponse) `shouldBe` Just (Unlocked untilTime)
                    liftIO $ sleep 2
                    secondResponse <- get "/state"
                    liftIO $ decode (simpleBody secondResponse) `shouldBe` Just (Unlocked untilTime)
                    liftIO $ sleep 3
                    thirdResponse <- get "/state"
                    liftIO $ decode (simpleBody thirdResponse) `shouldBe` Just Locked
          with runApp $
            do it "overrides current unlocks that expire earlier" $
                 do now <- liftIO getCurrentTime
                    let untilTime = 3.0 `addUTCTime` now
                    _ <- putJSON "/state" (encode $ Unlocked untilTime)
                    liftIO $ sleep 1
                    let laterUntilTime = 7.0 `addUTCTime` now
                    firstResponse <- putJSON "/state" (encode $ Unlocked laterUntilTime)
                    liftIO $ decode (simpleBody firstResponse) `shouldBe` Just (Unlocked laterUntilTime)
                    liftIO $ sleep 9
                    secondResponse <- get "/state"
                    liftIO $ decode (simpleBody secondResponse) `shouldBe` Just Locked

     describe "Locking" $
       do with runApp $
            do it "overrides unlocks" $
                 do now <- liftIO $ getCurrentTime
                    let untilTime = 20.0 `addUTCTime` now
                    _ <- putJSON "/state" (encode $ Unlocked untilTime)
                    liftIO $ sleep 1
                    response <- putJSON "/state" (encode Locked)
                    liftIO $ decode (simpleBody response) `shouldBe` Just Locked

     describe "Docs application" $
       do with runDocsApp $
            do it "serves docs" $
                 get "/docs" `shouldRespondWith` 200 { matchHeaders = [hContentType <:> "text/plain"]}