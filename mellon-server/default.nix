{ mkDerivation, aeson, base, either, hspec, lucid, mellon
, optparse-applicative, servant, servant-docs, servant-lucid
, servant-server, stdenv, text, time, transformers, wai, warp
}:
mkDerivation {
  pname = "mellon-server";
  version = "0.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base either lucid mellon optparse-applicative servant
    servant-docs servant-lucid servant-server text time transformers
    wai warp
  ];
  executableHaskellDepends = [
    aeson base either lucid mellon optparse-applicative servant
    servant-docs servant-lucid servant-server text time transformers
    wai warp
  ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
