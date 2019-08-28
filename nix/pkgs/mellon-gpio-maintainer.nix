{ mkDerivation, base, hlint, hpack, hpio, mellon-core, protolude
, stdenv
}:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.8.0.7";
  src = ../../mellon-gpio;
  configureFlags = [ "-ftest-hlint" ];
  libraryHaskellDepends = [ base hpio mellon-core protolude ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ base hlint protolude ];
  prePatch = "hpack";
  homepage = "https://github.com/dhess/mellon#readme";
  description = "GPIO support for mellon";
  license = stdenv.lib.licenses.bsd3;
}
