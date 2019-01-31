## This builds just mellon-* (plus hlint tests) for the current
## system. It's useful for development and interactive testing.

let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "aarch64-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [ (import ../../.) ];
  }
}:

with import (fixedNixPkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

  jobs = (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  mellon-core = jobs.haskellPackages.mellon-core-all-tests.${builtins.currentSystem};
  mellon-gpio = jobs.haskellPackages.mellon-gpio-all-tests.${builtins.currentSystem};
  mellon-web = jobs.haskellPackages.mellon-web-all-tests.${builtins.currentSystem};
}
