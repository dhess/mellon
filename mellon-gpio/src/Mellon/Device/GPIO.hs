{-|
Module      : Mellon.Device.GPIO
Description : GPIO-driven @mellon-core@ devices
Copyright   : (c) 2017, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

module Mellon.Device.GPIO
         ( -- * Mellon GPIO device constructors
           sysfsGpioDevice
         ) where

import Mellon.Device (Device(..))
import System.GPIO.Monad (PinValue(..), OutputPin, writeOutputPin)
import System.GPIO.Linux.Sysfs (PinDescriptor, runSysfsGpioIO)

-- | Given an 'OutputPin' on a Linux @sysfs@-based GPIO system, create
-- a @mellon-core@ 'Device' such that, when the
-- 'Mellon.Controller.Controller' locks the device, it drives a 'High'
-- value on the pin, and when it unlocks the device, it drives a 'Low'
-- value.
--
-- Note that the value driven on the pin is its /logical value/. You
-- can invert the signal levels by configuring the active level of the
-- 'OutputPin'.
sysfsGpioDevice :: OutputPin PinDescriptor -> Device PinDescriptor
sysfsGpioDevice p =
  Device (runSysfsGpioIO $ writeOutputPin p Low)
         (runSysfsGpioIO $ writeOutputPin p High)
