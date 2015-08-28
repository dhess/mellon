# mellon

"Speak, friend, and enter."

mellon is a Haskell package for controlling physical access devices,
such as electric strikes and other electronic locks.

mellon is a low-level package that provides, among other things:

* The <code>LockDevice</code> typeclass, for interfacing with a
physical device driver or other low-level lock interface.

* The <code>MonadController</code> typeclass, which provides a monadic
interface for controlling a device from an application or service.

* <code>ConcurrentController</code>, a controller implementation that
  works with multiple simultaneous control threads. This is useful for
  controlling a lock from a network service, for example.

Note that mellon does not provide any useful <code>LockDevice</code>
implementations; that is the domain of access device
implementation-specific packages. Similarly, mellon does not provide
authentication mechanisms or network services; that is the domain of
higher-level packages which use the base mellon package.
