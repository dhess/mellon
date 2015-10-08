# mellon-server

`mellon-server` wraps a `mellon` controller in a REST web service.
Like the `mellon` controller interface, the `mellon-server` web API is
quite simple. There are only 3 methods:

* `GET /time` returns the system time on the server. This is made
  available for diagnostic purposes, primarily to ensure the server
  has an accurate clock.

* `GET /state` returns the controller's current state (either `Locked`
  or `Unlocked date` where `date` is the UTC time at which the
  controller will automatically lock again).

* `PUT /state` sets the controller's current state. Use this method to
  lock and unlock the controller.

See [API.md](API.md) for detailed documentation on the REST service.

Note that `mellon-server` does not provide an authentication
mechanism! You should proxy `mellon-server` behind a secure HTTPS
server such as Nginx.
