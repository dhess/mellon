{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, either, hspec
      , optparse-applicative, servant, servant-docs, servant-server
      , stdenv, time, transformers, wai, warp
      }:
      mkDerivation {
        pname = "mellon-server";
        version = "0.0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base either optparse-applicative servant servant-docs
          servant-server time transformers wai warp
        ];
        executableHaskellDepends = [
          aeson base either optparse-applicative servant servant-docs
          servant-server time transformers wai warp
        ];
        testHaskellDepends = [ base hspec ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
