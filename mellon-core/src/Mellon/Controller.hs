{-|
Module      : Mellon.Controller
Description : The default @mellon-core@ controller
Copyright   : (c) 2019, Drew Hess
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

A controller's behavior is determined by the @mellon-core@ state
machine. See the "Mellon.StateMachine" module for a detailed
description of the state machine's operation.

This module re-exports the default (and, currently, only) controller
implementation.

-}

module Mellon.Controller
       ( module Mellon.Controller.Async
       ) where

import Mellon.Controller.Async
