# mellon-gpio

`mellon-gpio` adapts the `mellon-core` `Device` type to GPIO, for use
with a `mellon-core` controller.

To use it, simply connect a properly-designed physical access device
(e.g., an electric strike driven by a relay circuit such as the one
shown
[here](http://www.petervis.com/Raspberry_PI/Driving_Relays_with_CMOS_and_TTL_Outputs/Driving_Relays_with_CMOS_and_TTL_Outputs.html))
to an available GPIO pin on the host where the `mellon-core`
application will run.

[![Travis CI build status](https://travis-ci.org/quixoftic/mellon.svg?branch=master)](https://travis-ci.org/quixoftic/mellon)
