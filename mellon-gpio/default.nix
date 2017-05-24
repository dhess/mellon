{ mkDerivation, base, hlint, hpio, mellon-core, stdenv }:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.7.1.0";
  src = ./.;
  libraryHaskellDepends = [ base hpio mellon-core ];
  testHaskellDepends = [ base hlint ];
  homepage = "https://github.com/quixoftic/mellon/";
  description = "GPIO support for mellon";
  license = stdenv.lib.licenses.bsd3;
}
