{ mkDerivation, base, hlint, hpio, mellon-core, protolude, stdenv
}:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.8.0.5";
  src = ../../mellon-gpio;
  configureFlags = [ "-ftest-hlint" ];
  libraryHaskellDepends = [ base hpio mellon-core protolude ];
  testHaskellDepends = [ base hlint protolude ];
  homepage = "https://github.com/quixoftic/mellon#readme";
  description = "GPIO support for mellon";
  license = stdenv.lib.licenses.bsd3;
}
