{ mkDerivation, base, free, hspec, mtl, stdenv, text, time
, transformers
}:
mkDerivation {
  pname = "mellon";
  version = "0.6.0.1";
  src = ./.;
  libraryHaskellDepends = [ base free mtl text time transformers ];
  testHaskellDepends = [
    base free hspec mtl text time transformers
  ];
  homepage = "https://github.com/dhess/mellon/";
  description = "Control physical access devices";
  license = stdenv.lib.licenses.bsd3;
}
