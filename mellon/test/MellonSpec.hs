module MellonSpec (main, spec) where

import qualified Control.Concurrent as CC (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import qualified Data.Time as Time (getCurrentTime)
import Mellon.Monad.Controller (MonadController(..), ControllerT, controllerCtx, runControllerT)
import Mellon.Device.MockLock (MockLockEvent(..), events, mockLock)
import Test.Hspec

main :: IO ()
main = hspec spec

sleep :: MonadIO m => Int -> m ()
sleep = liftIO . CC.threadDelay . (* 1000000)

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

timePlusN :: UTCTime -> Integer -> UTCTime
timePlusN time n = (fromInteger n) `addUTCTime` time

type TestController m a = WriterT [MockLockEvent] (ControllerT m) a

testController :: (MonadIO m) => ControllerT m [MockLockEvent]
testController =
  do expectedResults <- execWriterT theTest
     return expectedResults

  where theTest :: (MonadIO m) => TestController m ()
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

        lockIt :: (MonadIO m) => TestController m ()
        lockIt =
          do now <- getCurrentTime
             _ <- lockNow
             tell [LockEvent now]

        unlock_ :: (MonadIO m) => Integer -> TestController m (UTCTime, UTCTime)
        unlock_ duration =
          do now <- getCurrentTime
             let expire = timePlusN now duration
             _ <- unlockUntil expire
             return (now, expire)

        unlockWillExpire :: (MonadIO m) => Integer -> TestController m ()
        unlockWillExpire duration =
          do (now, expire) <- unlock_ duration
             tell [UnlockEvent now]
             tell [LockEvent expire]

        unlockWontExpire :: (MonadIO m) => Integer -> TestController m ()
        unlockWontExpire duration =
          do (now, _) <- unlock_ duration
             tell [UnlockEvent now]

        unlockWillBeIgnored :: (MonadIO m) => Integer -> TestController m ()
        unlockWillBeIgnored duration =
          do _ <- unlock_ duration
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

concurrentControllerTest :: IO CheckedResults
concurrentControllerTest =
  do ml <- mockLock
     cc <- controllerCtx ml
     ccEvents <- runControllerT cc testController
     -- Discard the first MockLock event, which happened when
     -- concurrentController initialized the lock.
     _:lockEvents <- events ml
     return $ checkResults ccEvents lockEvents (0.5 :: NominalDiffTime)

spec :: Spec
spec = do
  describe "concurrentController test" $ do
    it "should produce the correct lock sequence plus or minus a few hundred milliseconds" $ do
      concurrentControllerTest >>= (`shouldBe` Right "OK")

