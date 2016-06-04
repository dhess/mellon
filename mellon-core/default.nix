{ mkDerivation, base, doctest, hlint, hspec, QuickCheck
, quickcheck-instances, stdenv, time
}:
mkDerivation {
  pname = "mellon-core";
  version = "0.6.0.1";
  src = ./.;
  libraryHaskellDepends = [ base time ];
  testHaskellDepends = [
    base doctest hlint hspec QuickCheck quickcheck-instances time
  ];
  homepage = "https://github.com/dhess/mellon/";
  description = "Control physical access devices";
  license = stdenv.lib.licenses.bsd3;
}
