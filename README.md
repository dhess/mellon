# Mellon

<em>"Speak, friend, and enter."</em>

Mellon is a set of Haskell packages for controlling physical access
devices, such as electric strikes and other electronic locks. The set
of packages includes:

* `mellon-core`, which provides the core functionality upon which the
  other packages depend.

* `mellon-gpio`, for controlling physical access devices via GPIO.

* `mellon-web`, a web interface (with client and server bindings) for
  interacting with Mellon controllers.

## Building from git

You will need [bumper](https://hackage.haskell.org/package/bumper) in
order to use the `bump-versions.sh` script.

With `nix`:
```shell
./scripts/generate-nix-files.sh   # Get up-to-date shell.nix files
```

With `stack`:
```shell
stack build
```
