{ mkDerivation, base, exceptions, gpio, mellon, mellon-server, mtl
, network, optparse-applicative, stdenv, transformers, warp
}:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.6";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exceptions gpio mellon mellon-server mtl network transformers
    warp
  ];
  executableHaskellDepends = [
    base exceptions gpio mellon mellon-server mtl network
    optparse-applicative transformers warp
  ];
  homepage = "https://github.com/dhess/mellon/";
  description = "Run a mellon controller using GPIO";
  license = stdenv.lib.licenses.bsd3;
}
