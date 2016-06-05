{ mkDerivation, base, doctest, hlint, hspec, mtl, QuickCheck
, quickcheck-instances, stdenv, time, transformers
}:
mkDerivation {
  pname = "mellon-core";
  version = "0.6.0.1";
  src = ./.;
  libraryHaskellDepends = [ base mtl time transformers ];
  testHaskellDepends = [
    base doctest hlint hspec mtl QuickCheck quickcheck-instances time
    transformers
  ];
  homepage = "https://github.com/dhess/mellon/";
  description = "Control physical access devices";
  license = stdenv.lib.licenses.bsd3;
}
