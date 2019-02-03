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

  jobs = {
    nixpkgs = pkgs.releaseTools.aggregate {
      name = "nixpkgs";
      meta.description = "mellon packages built against nixpkgs haskellPackages";
      meta.maintainers = pkgs.lib.maintainers.dhess-pers;
      constituents = with jobs; [
        haskellPackages.mellon-core-all-tests.x86_64-darwin
        haskellPackages.mellon-core-all-tests.x86_64-linux
        haskellPackages.mellon-core-all-tests.aarch64-linux
        haskellPackages.mellon-gpio-all-tests.x86_64-darwin
        haskellPackages.mellon-gpio-all-tests.x86_64-linux
        haskellPackages.mellon-gpio-all-tests.aarch64-linux
        haskellPackages.mellon-web-all-tests.x86_64-darwin
        haskellPackages.mellon-web-all-tests.x86_64-linux
        haskellPackages.mellon-web-all-tests.aarch64-linux
      ];
    };
  } // (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  inherit (jobs) nixpkgs;
}
