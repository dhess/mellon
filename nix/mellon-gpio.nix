{ mkDerivation, base, hlint, hpio, mellon-core, stdenv }:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.8.0.2";
  src = ../mellon-gpio;
  libraryHaskellDepends = [ base hpio mellon-core ];
  testHaskellDepends = [ base hlint ];
  homepage = "https://github.com/quixoftic/mellon#readme";
  description = "GPIO support for mellon";
  license = stdenv.lib.licenses.bsd3;
}
