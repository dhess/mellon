{ mkDerivation, base, free, hspec, mtl, optparse-applicative
, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "mellon";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base free mtl optparse-applicative text time transformers
  ];
  testDepends = [ base free hspec mtl text time transformers ];
  homepage = "https://github.com/dhess/mellon/";
  description = "Control physical access devices";
  license = stdenv.lib.licenses.bsd3;
}
