{ mkDerivation, async, base, doctest, hlint, hspec, mtl, QuickCheck
, quickcheck-instances, stdenv, time, transformers
}:
mkDerivation {
  pname = "mellon-core";
  version = "0.8.0.1";
  src = ./.;
  libraryHaskellDepends = [ async base mtl time transformers ];
  testHaskellDepends = [
    async base doctest hlint hspec mtl QuickCheck quickcheck-instances
    time transformers
  ];
  homepage = "https://github.com/quixoftic/mellon/";
  description = "Control physical access devices";
  license = stdenv.lib.licenses.bsd3;
}
