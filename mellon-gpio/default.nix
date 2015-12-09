{ mkDerivation, base, gpio, mellon, mellon-server, mtl, network
, optparse-applicative, stdenv, warp
}:
mkDerivation {
  pname = "mellon-gpio";
  version = "0.6";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base gpio mellon mellon-server mtl network warp
  ];
  executableHaskellDepends = [
    base gpio mellon mellon-server mtl network optparse-applicative
    warp
  ];
  homepage = "https://github.com/dhess/mellon/";
  description = "Run a mellon controller using GPIO";
  license = stdenv.lib.licenses.bsd3;
}
