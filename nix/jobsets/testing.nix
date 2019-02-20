## This builds just mellon (in maintainer mode) for the current system.
## It's useful for development and interactive testing.

let

  fixedNixpkgs = (import ../lib).fixedNixpkgs;
  localPkgs = (import ../..) {};

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "aarch64-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [ localPkgs.overlays.mellonMaintainer ];
  }
}:

with import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

  jobs = (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  mellon-core = jobs.haskellPackages.mellon-core.${builtins.currentSystem};
  mellon-gpio = jobs.haskellPackages.mellon-gpio.${builtins.currentSystem};
  mellon-web = jobs.haskellPackages.mellon-web.${builtins.currentSystem};
}
