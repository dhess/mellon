module System.Mellon.StateMachine
         ( LockState
         , Command
         , runCommand
         ) where

import Data.Time.Clock (UTCTime(..))

data LockState
  = Locked
  | Unlocked UTCTime
  deriving (Eq)

data Command
  = Lock
  | Unlock UTCTime
  deriving (Eq)

runCommand :: Command -> LockState -> LockState

runCommand Lock _ = Locked

runCommand (Unlock untilDate) Locked = Unlocked untilDate

runCommand (Unlock untilDate) (Unlocked scheduledDate) =
  if untilDate > scheduledDate
     then (Unlocked untilDate)
     else (Unlocked scheduledDate)
