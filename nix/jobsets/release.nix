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

  all = pkg: pkgs.lib.testing.enumerateSystems pkg supportedSystems;

  jobs = {
    nixpkgs = pkgs.releaseTools.aggregate {
      name = "nixpkgs";
      meta.description = "mellon packages built against nixpkgs haskellPackages";
      meta.maintainers = pkgs.lib.maintainers.dhess-pers;
      constituents = with jobs; [
        (all haskellPackages.mellon-core-all-tests)
        (all haskellPackages.mellon-gpio-all-tests)
        (all haskellPackages.mellon-web-all-tests)
      ];
    };
  } // (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  inherit (jobs) nixpkgs;
  inherit (jobs.haskellPackages) mellon-core-all-tests;
  inherit (jobs.haskellPackages) mellon-gpio-all-tests;
  inherit (jobs.haskellPackages) mellon-web-all-tests;
}
