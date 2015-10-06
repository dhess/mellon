# mellon-server

`mellon-server` wraps a `mellon` controller in a web service. Like the
`mellon` controller interface, the `mellon-server` web API is quite
simple. There are only 3 methods:

* `GET /time` returns the system time on the server. This is made
  available for diagnostic purposes, primarily to ensure the server
  has an accurate clock.

* `GET /state` returns the controller's current state (either `Locked`
  or `Unlocked date` where `date` is the UTC time at which the
  controller will automatically lock again).

* `PUT /state` sets the controller's current state. Use this method to
  lock and unlock the controller.

See the package documentation for details on the `Content-Type`s and
response body formats.
