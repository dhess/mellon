{ mkDerivation, base, doctest, hlint, hpio, mellon-core, stdenv }:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.6.0.1";
  src = ./.;
  libraryHaskellDepends = [ base hpio mellon-core ];
  testHaskellDepends = [ base doctest hlint ];
  homepage = "https://github.com/dhess/mellon/";
  description = "GPIO support for mellon";
  license = stdenv.lib.licenses.bsd3;
}
