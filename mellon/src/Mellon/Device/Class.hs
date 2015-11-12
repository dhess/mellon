-- | A typeclass for physical access devices.
--
-- Typically, a device will be some sort of system-level interface to
-- a physical lock control, often a device driver written in a
-- low-level language such as C; e.g., a device driver or kernel-level
-- GPIO API for controlling an electric strike. Therefore, devices run
-- in the 'IO' monad.

module Mellon.Device.Class
         ( Device (..)
         ) where

-- | An interface to physical access devices.
class Device l where
  -- | Lock the device immediately.
  lockDevice :: l -> IO ()
  -- | Unlock the device immediately.
  unlockDevice :: l -> IO ()
