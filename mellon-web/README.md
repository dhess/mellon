# mellon-web

The `mellon-web` package wraps a `mellon-core` controller in a REST
web service, making it possible to control physical access devices
from an HTTP client. The package includes both a WAI application
server, and native Haskell client bindings for the service.

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

## Example servers

### "Mock" server

An extremely simple example server (with on-line documentation
support) is provided in the `examples` directory. You can run it with
`cabal run mock-mellon-server` and test it using the endpoints
described in [API.md](API.md). The server is will run on the
`localhost` loopback interface on port 8081.

Also included is a [Paw](https://luckymarmot.com/paw) file with a
pre-defined `localhost` environment for use with the example server.

This particular example server uses a "mock lock" device which only
internally logs lock and unlock events without depending on any actual
hardware, so it will run anywhere.

### GPIO server

Another included example server uses the `mellon-gpio` package to
drive a simple physical access device via a GPIO pin. This server must
be run on a Linux host with GPIO hardware, e.g., a Raspberry Pi
running Linux.

This server takes a GPIO pin number and a local port number, then
starts a `mellon-web` server on all local interfaces on the specified
port. When the server receives an unlock request, it will drive a high
signal on the specified GPIO pin. When the unlock expires, or when the
server receives a lock request, it will drive a low signal on the
specified GPIO pin.

To use this server, simply connect a properly-designed physical access
device (e.g., an electric strike driven by a relay circuit such as the
one shown
[here](http://www.petervis.com/Raspberry_PI/Driving_Relays_with_CMOS_and_TTL_Outputs/Driving_Relays_with_CMOS_and_TTL_Outputs.html))
to an available GPIO pin on the host device, then run the server with
the specified GPIO pin number and port. For example, to run the server
on port 7533 using GPIO pin 65:

```
cabal run gpio-mellon-server -- sysfs --port 7533 65
```

The `sysfs` command tells the server to use the Linux `sysfs` GPIO
interpreter. (Currently, this is the only supported GPIO platform.)

**NOTE**: the REST service provided by `gpio-mellon-server` offers no
security/authentication for your access control device! You should
always run it (or any `mellon-web` server) behind a secure proxy web
service or equivalent HTTP(S)-based authentication mechanism.

[![Travis CI build status](https://travis-ci.org/dhess/mellon.svg?branch=master)](https://travis-ci.org/dhess/mellon)
