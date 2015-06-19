with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, free, hspec, optparse-applicative, stdenv
             , time
             }:
             mkDerivation {
               pname = "mellon";
               version = "0.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [ base free optparse-applicative time ];
               testDepends = [ base free hspec time ];
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
