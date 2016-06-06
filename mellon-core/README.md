# mellon-core

<em>"Speak, friend, and enter."</em>

`mellon-core` is a Haskell package for controlling physical access
devices, e.g., electric strikes. The access control protocol is quite
simple: a device is either locked, or it is unlocked until a
particular date and time (an <em>expiration date</em>). Once the
expiration date passes, the device is automatically locked again. In
the meantime, the device can be locked immediately, overriding the
unlocked state, or the unlock period can be extended.

User programs incorporate `mellon-core` functionality via a
`Controller`, which is responsible for handling user lock and unlock
commands, and for scheduling and canceling unlock expirations.

User programs must also adapt their physical access devices to the
interface expected by the controller. For this purpose, `mellon-core`
defines a generic `Device` parametric data type with 2 simple `IO`
actions for locking and unlocking the device. (`mellon-core` does not
provide any useful device implementations; see the companion
`mellon-gpio` package for a GPIO-driven implementation.)

Note that `mellon-core` does not provide authentication mechanisms or
network services for interacting with controllers; that is the domain
of higher-level packages which use the base `mellon-core` package
(e.g., `mellon-web`).
