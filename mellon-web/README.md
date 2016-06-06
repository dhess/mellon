# mellon-web

The `mellon-web` package wraps a `mellon-core` controller in a REST
web service, and includes both a WAI application and native Haskell
client bindings for the service.

Like the `mellon-core` controller interface, the `mellon-web` REST API
is quite simple. There are only 3 methods:

* `GET /time` returns the system time on the server. This is made
  available for diagnostic purposes, primarily to ensure the server
  has an accurate clock.

* `GET /state` returns the controller's current state (either `Locked`
  or `Unlocked date` where `date` is the UTC time at which the
  controller will automatically lock again).

* `PUT /state` sets the controller's current state. Use this method to
  lock and unlock the controller.

See [API.md](API.md) for detailed documentation on the REST service.

Note that the `mellon-web` server does not provide an authentication
mechanism! You should proxy it behind a secure, authenticating HTTPS
server such as Nginx.

## Example server

An extremely simple example server (with on-line documentation
support) is provided in the `examples` directory. You can run it with
`cabal run` and test it using the endpoints described in
[API.md](API.md). A [Paw](https://luckymarmot.com/paw) file is also
included in the project with a pre-defined localhost environment for
use with the example server.
