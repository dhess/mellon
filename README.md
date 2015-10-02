# Mellon

<em>"Speak, friend, and enter."</em>

Mellon is a set of Haskell packages for controlling physical access
devices, such as electric strikes and other electronic locks. The set
of packages includes:

* `mellon`, which provides the core functionality upon
  which the other packages depend.

* `mellon-server`, a web interface for interacting with
  Mellon controllers.

## Building from git

You will need [bumper](https://hackage.haskell.org/package/bumper) in
order to use the `bump-versions.sh` script.

With `nix`:
```shell
./scripts/generate-nix-files.sh   # Get up-to-date shell.nix files
```

`stack` support coming soon.
