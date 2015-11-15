{ mkDerivation, base, HPi, mellon, mellon-server, network
, optparse-applicative, stdenv, warp
}:
mkDerivation {
  pname = "mellon-pi";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base HPi mellon mellon-server network warp
  ];
  executableHaskellDepends = [
    base HPi mellon mellon-server network optparse-applicative warp
  ];
  homepage = "https://github.com/dhess/mellon/";
  description = "Run a mellon controller on a Raspberry Pi";
  license = stdenv.lib.licenses.bsd3;
}
