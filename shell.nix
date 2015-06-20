with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, free, hspec, optparse-applicative, stdenv
             , text, time, transformers
             }:
             mkDerivation {
               pname = "mellon";
               version = "0.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 base free optparse-applicative text time transformers
               ];
               testDepends = [ base free hspec text time transformers ];
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
