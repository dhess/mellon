{ mkDerivation, base, hpack, hpio, mellon-core, protolude, stdenv
}:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.8.0.7";
  src = ../../mellon-gpio;
  libraryHaskellDepends = [ base hpio mellon-core protolude ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/dhess/mellon#readme";
  description = "GPIO support for mellon";
  license = stdenv.lib.licenses.bsd3;
}
