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

  enumerateConstituents = aggregate: lib.listToAttrs (
    map (d:
           let
             name = (builtins.parseDrvName d.name).name;
             system = d.system;
           in
             { name = name + "." + system;
               value = d;
             }
         )
        aggregate.constituents
  );

  jobs = {

    x86_64-darwin = pkgs.releaseTools.aggregate {
      name = "mellon-x86_64-darwin";
      meta.description = "mellon packages (x86_64-darwin)";
      constituents = with jobs; [
        haskellPackages.mellon-core.x86_64-darwin
        haskellPackages.mellon-gpio.x86_64-darwin
        haskellPackages.mellon-web.x86_64-darwin
      ];
    };

    x86_64-linux = pkgs.releaseTools.aggregate {
      name = "mellon-x86_64-linux";
      meta.description = "mellon packages (x86_64-linux)";
      constituents = with jobs; [
        haskellPackages.mellon-core.x86_64-linux
        haskellPackages.mellon-gpio.x86_64-linux
        haskellPackages.mellon-web.x86_64-linux
      ];
    };

  } // (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  inherit (jobs) x86_64-darwin;
  inherit (jobs) x86_64-linux;
}
// enumerateConstituents jobs.x86_64-darwin
// enumerateConstituents jobs.x86_64-linux
