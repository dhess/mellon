# mellon-gpio

`mellon-gpio` mixes `mellon-core` and `mellon-server` together with
the `gpio` package to create a turnkey physical access controller,
where the physical access device is controlled by a GPIO pin on the
hosting device. Simply connect a properly-designed physical access
device (e.g., an electric strike driven by a relay circuit such as the
one shown
[here](http://www.petervis.com/Raspberry_PI/Driving_Relays_with_CMOS_and_TTL_Outputs/Driving_Relays_with_CMOS_and_TTL_Outputs.html))
to an available GPIO pin on the host device, then run `mellon-gpio`
with the specified GPIO pin number and port to provide a REST web
service for your access device.

**NOTE**: the REST service provided by `mellon-gpio` offers no
security/authentication for your access control device! You should
always run `mellon-gpio` behind a secure proxy web service or equivalent
HTTP(S)-based authentication mechanism.
