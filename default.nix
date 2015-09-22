{ mkDerivation, aeson, base, hspec, optparse-applicative, servant
, servant-docs, servant-server, stdenv, time, wai, warp
}:
mkDerivation {
  pname = "mellon-server";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base optparse-applicative servant servant-docs servant-server
    time wai warp
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
