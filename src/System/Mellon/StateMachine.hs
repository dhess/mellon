module System.Mellon.StateMachine
         ( LockState
         , Command
         , runCommand
         ) where

import Data.Time.Clock (UTCTime(..))

data LockState
  = Locked
  | UnlockedUntil UTCTime
  deriving (Eq)

data Command
  = Lock
  | LockAt UTCTime
  | UnlockUntil UTCTime
  deriving (Eq)

runCommand :: Command -> LockState -> LockState

runCommand Lock _ = Locked

runCommand (LockAt _) Locked = Locked

runCommand (LockAt date) (UnlockedUntil scheduledDate) =
  if date >= scheduledDate
     then Locked
     else (UnlockedUntil scheduledDate)

runCommand (UnlockUntil date) Locked = UnlockedUntil date

runCommand (UnlockUntil date) (UnlockedUntil scheduledDate) =
  if date > scheduledDate
     then (UnlockedUntil date)
     else (UnlockedUntil scheduledDate)
