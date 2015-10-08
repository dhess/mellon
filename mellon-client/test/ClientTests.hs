{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ClientTests (spec, sleep) where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Time.Clock
import Mellon.Client
import Servant.API
import Servant.Client
import Test.Hspec

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

getTime :: EitherT ServantError IO Time
getState :: EitherT ServantError IO State
putState :: State -> EitherT ServantError IO State

getTime :<|> getState :<|> putState = generateClientFunctions host
  where host = (BaseUrl Http "localhost" 8081)

spec :: Spec
spec =
  -- We assume these tests run in the specified order, as the same
  -- server is used for each test and the server obviously is
  -- stateful.
  do describe "Initial getState state" $ do
       it "is Locked" $ do
         Right state <- runEitherT getState
         state `shouldBe` Locked

     describe "getTime" $ do
       it "returns the proper serverTime" $ do
         now <- getCurrentTime
         Right (Time serverTime) <- runEitherT getTime
         let delta = 1.0 :: NominalDiffTime
         ((serverTime `diffUTCTime` now) < delta) `shouldBe` True

     describe "putState Locked when already locked" $ do
       it "is idempotent" $ do
         Right state <- runEitherT $ putState Locked
         state `shouldBe` Locked

     describe "putState (Unlocked now+3s)" $ do
       it "unlocks for 3 seconds" $ do
         now <- getCurrentTime
         let untilTime = 3.0 `addUTCTime` now
         Right newState <- runEitherT $ putState (Unlocked untilTime)
         newState `shouldBe` (Unlocked untilTime)

       it "then the server automatically locks again" $ do
         sleep 3
         Right nextState <- runEitherT getState
         nextState `shouldBe` Locked

       it "overrides unlocks that expire earlier" $ do
         now <- getCurrentTime
         let untilTime = 3.0 `addUTCTime` now
         Right newState1 <- runEitherT $ putState (Unlocked untilTime)
         newState1 `shouldBe` (Unlocked untilTime)
         sleep 1
         let newUntilTime = 7.0 `addUTCTime` now
         Right newState2 <- runEitherT $ putState (Unlocked newUntilTime)
         newState2 `shouldBe` (Unlocked newUntilTime)
         sleep 3
         Right nextState <- runEitherT getState
         nextState `shouldBe` newState2
         sleep 4
         Right newState3 <- runEitherT getState
         newState3 `shouldBe` Locked

     describe "putState Locked" $ do
       it "overrides unlocks" $ do
         now <- getCurrentTime
         let untilTime = 20.0 `addUTCTime` now
         _ <- runEitherT $ putState (Unlocked untilTime)
         sleep 1
         Right newState <- runEitherT $ putState Locked
         newState `shouldBe` Locked
