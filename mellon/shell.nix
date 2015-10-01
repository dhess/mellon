{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, free, hspec, mtl, stdenv, text, time
      , transformers
      }:
      mkDerivation {
        pname = "mellon";
        version = "0.1.1";
        src = ./.;
        libraryHaskellDepends = [ base free mtl text time transformers ];
        testHaskellDepends = [
          base free hspec mtl text time transformers
        ];
        homepage = "https://github.com/dhess/mellon/";
        description = "Control physical access devices";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
