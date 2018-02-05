{ mkDerivation, base, hpio, mellon-core, protolude, stdenv }:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.8.0.6";
  src = ../../mellon-gpio;
  libraryHaskellDepends = [ base hpio mellon-core protolude ];
  homepage = "https://github.com/quixoftic/mellon#readme";
  description = "GPIO support for mellon";
  license = stdenv.lib.licenses.bsd3;
}
