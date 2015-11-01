# mellon-pi

`mellon-pi` mixes `mellon` and `mellon-server` together with Raspberry
Pi/Raspberry Pi 2 GPIO to create a turnkey physical access controller.
Simply connect a properly-designed physical access device (e.g., an
electric strike driven by a relay circuit such as the one shown
[here](http://www.petervis.com/Raspberry_PI/Driving_Relays_with_CMOS_and_TTL_Outputs/Driving_Relays_with_CMOS_and_TTL_Outputs.html))
to an available GPIO pin on your RPi/RPi2, then run `mellon-pi` with
the specified GPIO pin number and port to provide a REST web service
for your access device.

**NOTE**: the REST service provided by `mellon-pi` offers no
security/authentication for your access control device! You should
always run `mellon-pi` behind a secure proxy web service or equivalent
HTTP(S)-based authentication mechanism.

## Requirements

`mellon-pi` uses the [HPi](https://hackage.haskell.org/package/HPi)
Haskell package to interact directly with the Raspberry Pi/Raspberry
Pi 2 BCM2835 SoC. As such, `mellon-pi` will only function on an
RPi/RPi2 device (or equivalent device with a BCM2835 or
BCM2835-compatible SoC).

However, the GPIO functionality required by `mellon-pi` to drive the
physical access device is quite simple: configure the GPIO pin for
output, drive it high when the `mellon` controller unlocks the device,
and drive it low when the device is locked. Accordingly, it should
be relatively easy to adapt the `mellon-pi` source to drive a
different, but equivalent, GPIO interface; e.g., the Linux
`/sys/class/gpio` interface could be used directly, as described in
the
[Linux kernel documentation](https://www.kernel.org/doc/Documentation/gpio/sysfs.txt)
