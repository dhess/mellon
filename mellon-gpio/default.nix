{ mkDerivation, base, hlint, hpio, mellon-core, stdenv }:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.7.0.3";
  src = ./.;
  libraryHaskellDepends = [ base hpio mellon-core ];
  testHaskellDepends = [ base hlint ];
  homepage = "https://github.com/dhess/mellon/";
  description = "GPIO support for mellon";
  license = stdenv.lib.licenses.bsd3;
}
