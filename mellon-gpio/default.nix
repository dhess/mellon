{ mkDerivation, base, doctest, exceptions, hlint, hpio, mellon-core
, mellon-web, mtl, network, optparse-applicative, stdenv
, transformers, warp
}:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.6.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exceptions hpio mellon-core mellon-web mtl network
    transformers warp
  ];
  executableHaskellDepends = [
    base exceptions hpio mellon-core mellon-web mtl network
    optparse-applicative transformers warp
  ];
  testHaskellDepends = [ base doctest hlint ];
  homepage = "https://github.com/dhess/mellon/";
  description = "Run a mellon controller using GPIO";
  license = stdenv.lib.licenses.bsd3;
}
