module Mellon.ControllerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (RWST, execRWST, ask, tell)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import qualified Data.Time as Time (getCurrentTime)
import Test.Hspec

import Mellon.Controller
       (Controller, controller, lockController, unlockController)
import Mellon.Device
       (MockLock, MockLockEvent(..), events, mockLock, mockLockDevice)

sleep :: (MonadIO m) => Int -> m ()
sleep = liftIO . threadDelay . (* 1000000)

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

timePlusN :: UTCTime -> Integer -> UTCTime
timePlusN time n = (fromInteger n) `addUTCTime` time

type TestController d a = RWST (Controller d) [MockLockEvent] () IO a

testController :: Controller d -> IO [MockLockEvent]
testController cc =
  do (_, expectedResults) <- execRWST theTest cc ()
     return expectedResults

  where theTest :: TestController d ()
        theTest =
          do unlockWillExpire 5
             sleep 8
             unlockWontExpire 3
             sleep 1
             unlockWillExpire 10
             sleep 14
             unlockWillExpire 8
             sleep 2
             unlockWillBeIgnored 1
             sleep 13
             unlockWontExpire 8
             sleep 3
             lockIt
             sleep 12

        lockIt :: TestController d ()
        lockIt =
          do now <- getCurrentTime
             cc <- ask
             void $ lockController cc
             tell [LockEvent now]

        unlockIt :: Integer -> TestController d (UTCTime, UTCTime)
        unlockIt duration =
          do now <- getCurrentTime
             cc <- ask
             let expire = timePlusN now duration
             void $ unlockController expire cc
             return (now, expire)

        unlockWillExpire :: Integer -> TestController d ()
        unlockWillExpire duration =
          do (now, expire) <- unlockIt duration
             tell [UnlockEvent now]
             tell [LockEvent expire]

        unlockWontExpire :: Integer -> TestController d ()
        unlockWontExpire duration =
          do (now, _) <- unlockIt duration
             tell [UnlockEvent now]

        unlockWillBeIgnored :: Integer -> TestController d ()
        unlockWillBeIgnored duration =
          do _ <- unlockIt duration
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
     cc <- controller $ mockLockDevice ml
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
