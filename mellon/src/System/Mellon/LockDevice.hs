-- | A typeclass for lock devices.
--
-- Typically, a lock device will be some sort of system-level
-- interface to a physical lock control, often a device driver written
-- in a low-level language such as C; e.g., a device driver or
-- kernel-level GPIO API for controlling an electric strike.
-- Therefore, the 'LockDevice' interface runs in the 'IO' monad.

module System.Mellon.LockDevice
         ( LockDevice(..)
         ) where

-- | An interface to lock devices.
class LockDevice l where
  -- | Lock the device immediately.
  lockDevice :: l -> IO ()
  -- | Unlock the device immediately.
  unlockDevice :: l -> IO ()
