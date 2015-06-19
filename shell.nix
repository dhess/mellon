with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, hspec, optparse-applicative, stdenv }:
             mkDerivation {
               pname = "mellon";
               version = "0.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [ base optparse-applicative ];
               testDepends = [ base hspec ];
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
