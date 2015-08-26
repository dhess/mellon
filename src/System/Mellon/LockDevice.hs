module System.Mellon.LockDevice
         ( LockDevice(..)
         ) where

class LockDevice l where
  lockDevice :: l -> IO ()
  unlockDevice :: l -> IO ()
