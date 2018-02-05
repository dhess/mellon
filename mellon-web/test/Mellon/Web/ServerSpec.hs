{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mellon.Web.ServerSpec (spec) where

import Protolude hiding (State, get)
import Control.Concurrent (threadDelay)
import Data.Aeson (decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)
import Data.Maybe (fromJust)
import Data.Time.Clock
import Mellon.Controller (controller)
import Mellon.Device (mockLock, mockLockDevice)
import Network.HTTP.Types (hContentType, methodPut)
import Network.Wai
import Network.Wai.Test (SResponse, simpleBody)
import Test.Hspec
import Test.Hspec.Wai ((<:>), WaiSession, get, liftIO, matchHeaders, request, shouldRespondWith, with)
import Test.QuickCheck
       (Arbitrary(..), genericShrink, oneof, property)
import Test.QuickCheck.Instances ()

import Mellon.Web.Server (State(..), Time(..), app, swaggerApp)

instance Arbitrary State where
  arbitrary = oneof [pure Locked, Unlocked <$> arbitrary]
  shrink = genericShrink

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary
  shrink = genericShrink

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

runApp :: IO Application
runApp =
  do ml <- mockLock
     cc <- controller Nothing $ mockLockDevice ml
     return (app cc)

runSwaggerApp :: IO Application
runSwaggerApp =
  do ml <- mockLock
     cc <- controller Nothing $ mockLockDevice ml
     return (swaggerApp cc)

putJSON :: ByteString -> LB.ByteString -> WaiSession SResponse
putJSON path = request methodPut path [(hContentType, "application/json;charset=utf-8")]

spec :: Spec
spec =
  do describe "ToJSON/FromJSON" $
       it "is isomorphic" $ property $
         \(s :: State) -> fromJust (decode $ encode s) == s

     describe "Server time" $
       do with runApp $
            do it "should be accurate" $
                 do now <- liftIO getCurrentTime
                    response <- get "/time"
                    let Just (serverTime :: UTCTime) = decode $ simpleBody response
                    let delta = 1.0 :: NominalDiffTime
                    liftIO $ ((serverTime `diffUTCTime` now < delta)) `shouldBe` True

     describe "Initial server state" $
       do ml <- runIO $ mockLock
          cc <- runIO $ controller Nothing $ mockLockDevice ml
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

#ifdef ENABLE_TIMING_SENSITIVE_TESTS
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
#endif

     describe "Locking" $
       do with runApp $
            do it "overrides unlocks" $
                 do now <- liftIO $ getCurrentTime
                    let untilTime = 20.0 `addUTCTime` now
                    _ <- putJSON "/state" (encode $ Unlocked untilTime)
                    liftIO $ sleep 1
                    response <- putJSON "/state" (encode Locked)
                    liftIO $ decode (simpleBody response) `shouldBe` Just Locked

     describe "Swagger application" $
       do with runSwaggerApp $
            do it "serves the Swagger UI" $
                 get "/swagger-ui/" `shouldRespondWith` 200 { matchHeaders = [hContentType <:> "text/html;charset=utf-8"]}
