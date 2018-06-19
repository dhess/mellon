let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" ]
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
        haskellPackages.mellon-gpio.x86_64-darwin
        haskellPackages.mellon-gpio.x86_64-linux
        haskellPackages.mellon-web.x86_64-darwin
        haskellPackages.mellon-web.x86_64-linux
      ];
    };

    lts-11 = pkgs.releaseTools.aggregate {
      name = "lts-11";
      meta.description = "mellon packages built against Stackage LTS 11 pacakge set";
      meta.maintainers = pkgs.lib.maintainers.dhess-qx;
      constituents = with jobs; [
        lts11Packages.mellon-core.x86_64-darwin
        lts11Packages.mellon-core.x86_64-linux
        lts11Packages.mellon-gpio.x86_64-darwin
        lts11Packages.mellon-gpio.x86_64-linux
        lts11Packages.mellon-web.x86_64-darwin
        lts11Packages.mellon-web.x86_64-linux
      ];
    };

    ghc843 = pkgs.releaseTools.aggregate {
      name = "ghc843";
      meta.description = "mellon packages built against nixpkgs haskellPackages using GHC 8.4.3";
      meta.maintainers = pkgs.lib.maintainers.dhess-qx;
      constituents = with jobs; [
        haskellPackages843.mellon-core.x86_64-darwin
        haskellPackages843.mellon-core.x86_64-linux
        haskellPackages843.mellon-gpio.x86_64-darwin
        haskellPackages843.mellon-gpio.x86_64-linux
        haskellPackages843.mellon-web.x86_64-darwin
        haskellPackages843.mellon-web.x86_64-linux
      ];
    };

  } // (mapTestOn ({

    haskellPackages = packagePlatforms pkgs.haskellPackages;
    haskellPackages843 = packagePlatforms pkgs.haskellPackages843;
    lts11Packages = packagePlatforms pkgs.lts11Packages;

  }));

in
{
  inherit (jobs) nixpkgs ghc843 lts-11;
}
