{-|
Module      : Mellon.Controller
Description : The default @mellon-core@ controller
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

In @mellon-core@, controllers are the intermediary between the
@mellon-core@ state machine, the physical access device, and the user
who wants to control the device. The user interacts directly only with
the controller, not with the physical access device or the state
machine.

A controller provides two commands to the user: /lock/ and /unlock/.
User lock commands are effective immediately, and the device remains
locked until the user runs a subsequent unlock command. User unlock
commands are effective immediately, but also take a 'UTCTime' argument
that specifies the date at which the controller will automatically
lock the device again.

The controller's behavior is determined by the @mellon-core@ state
machine. See the "Mellon.StateMachine" module for a detailed
description of the state machine's operation.

== Exception safety

All the controller actions provided in this module are exception-safe.
If an exception occurs in a controller action (e.g., because the
device throws an exception), the controller will be restored to its
state as it was immediately prior to the execution of the action, and
the exception will be re-thrown. After handling the exception, you can
continue to execute actions on the controller, if you wish. However,
the controller and the device may be out of sync at that point, or the
device may continue to throw exceptions until it can be reset.

The safest action to take after an exception occurs in a controller is
to reset the device to a known working state; and then to create, from
scratch, a new controller for the device.

-}

module Mellon.Controller
       ( -- * The default mellon-core controller
         Controller
       , controller
       , lockController
       , unlockController
       , queryController

         -- * Re-exported types
       , Device(..)
       , State(..)
       ) where

import Mellon.Controller.Async
       (Controller, Device(..), State(..), controller, lockController,
        unlockController, queryController)
