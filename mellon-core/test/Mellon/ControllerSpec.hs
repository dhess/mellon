module Mellon.ControllerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Strict (WriterT(..), execWriterT, tell)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import qualified Data.Time as Time (getCurrentTime)
import Test.Hspec

import Mellon.Controller
       (ControllerEnv, controllerEnv, controllerLock, controllerUnlock,
        controllerState)
import Mellon.Device
       (MockLock, MockLockEvent(..), events, mockLock, mockLockDevice)

sleep :: (MonadIO m) => Int -> m ()
sleep = liftIO . threadDelay . (* 1000000)

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

timePlusN :: UTCTime -> Integer -> UTCTime
timePlusN time n = (fromInteger n) `addUTCTime` time

type TestController a = WriterT [MockLockEvent] IO a

testController :: ControllerEnv d -> IO [MockLockEvent]
testController env =
  do expectedResults <- execWriterT $ theTest env
     return expectedResults

  where theTest :: ControllerEnv d -> TestController ()
        theTest env =
          do unlockWillExpire 5 env
             sleep 8
             unlockWontExpire 3 env
             sleep 1
             unlockWillExpire 10 env
             sleep 14
             unlockWillExpire 8 env
             sleep 2
             unlockWillBeIgnored 1 env
             sleep 13
             unlockWontExpire 8 env
             sleep 3
             lockIt env
             sleep 12

        lockIt :: ControllerEnv d -> TestController ()
        lockIt c =
          do now <- getCurrentTime
             void $ controllerLock c
             tell [LockEvent now]

        unlockIt :: Integer -> ControllerEnv d -> TestController (UTCTime, UTCTime)
        unlockIt duration c =
          do now <- getCurrentTime
             let expire = timePlusN now duration
             void $ controllerUnlock expire c
             return (now, expire)

        unlockWillExpire :: Integer -> ControllerEnv d -> TestController ()
        unlockWillExpire duration c =
          do (now, expire) <- unlockIt duration c
             tell [UnlockEvent now]
             tell [LockEvent expire]

        unlockWontExpire :: Integer -> ControllerEnv d -> TestController ()
        unlockWontExpire duration c =
          do (now, _) <- unlockIt duration c
             tell [UnlockEvent now]

        unlockWillBeIgnored :: Integer -> ControllerEnv d -> TestController ()
        unlockWillBeIgnored duration c =
          do _ <- unlockIt duration c
             return ()

type CheckedResults = Either ((MockLockEvent, MockLockEvent), String) String

checkResults :: [MockLockEvent]
             -> [MockLockEvent]
             -> NominalDiffTime
             -> CheckedResults
checkResults expected actual epsilon = foldr compareResult (Right "No results to compare") $ zip expected actual
  where compareResult :: (MockLockEvent, MockLockEvent) -> CheckedResults -> CheckedResults
        compareResult _ (Left l) = Left l
        compareResult ev@(UnlockEvent t1, UnlockEvent t2) _ =
          if t2 `diffUTCTime` t1 < epsilon
             then Right "OK"
             else Left (ev, "Time difference exceeds epsilon")
        compareResult ev@(LockEvent t1, LockEvent t2) _ =
          if t2 `diffUTCTime` t1 < epsilon
             then Right "OK"
             else Left (ev, "Time difference exceeds epsilon")
        compareResult ev _ = Left (ev, "Event types don't match")

controllerTest :: IO CheckedResults
controllerTest =
  do ml <- mockLock
     cc <- controllerEnv $ mockLockDevice ml
     ccEvents <- testController cc
     -- Discard the first MockLock event, which happened when
     -- concurrentController initialized the lock.
     _:lockEvents <- events ml
     return $ checkResults ccEvents lockEvents (0.5 :: NominalDiffTime)

spec :: Spec
spec = do
  describe "concurrentController test" $ do
    it "should produce the correct lock sequence plus or minus a few hundred milliseconds" $ do
      controllerTest >>= (`shouldBe` Right "OK")
