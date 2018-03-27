let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "armv7l-linux" ]
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
      meta.maintainers = pkgs.lib.maintainers.dhess-qx;
      constituents = with jobs; [
        haskellPackages.mellon-core.x86_64-darwin
        haskellPackages.mellon-core.x86_64-linux
        haskellPackagesArmv7l.mellon-core.armv7l-linux
        haskellPackages.mellon-gpio.x86_64-darwin
        haskellPackages.mellon-gpio.x86_64-linux
        haskellPackagesArmv7l.mellon-gpio.armv7l-linux
        haskellPackages.mellon-web.x86_64-darwin
        haskellPackages.mellon-web.x86_64-linux
        haskellPackagesArmv7l.mellon-web.armv7l-linux
      ];
    };

    ghc841 = pkgs.releaseTools.aggregate {
      name = "ghc841";
      meta.description = "mellon packages built against nixpkgs haskellPackages using GHC 8.4.1";
      meta.maintainers = pkgs.lib.maintainers.dhess-qx;
      constituents = with jobs; [
        haskellPackages841.mellon-core.x86_64-darwin
        haskellPackages841.mellon-core.x86_64-linux
        haskellPackages841.mellon-gpio.x86_64-darwin
        haskellPackages841.mellon-gpio.x86_64-linux
        haskellPackages841.mellon-web.x86_64-darwin
        haskellPackages841.mellon-web.x86_64-linux
      ];
    };

  } // (mapTestOn ({

    haskellPackages = packagePlatforms pkgs.haskellPackages;
    haskellPackages841 = packagePlatforms pkgs.haskellPackages841;
    haskellPackagesArmv7l = packagePlatforms pkgs.haskellPackagesArmv7l;

  }));

in
{
  inherit (jobs) nixpkgs ghc841;
}
