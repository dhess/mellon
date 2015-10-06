{ mkDerivation, aeson, base, either, hspec, http-client, http-types
, lucid, mellon, servant, servant-docs, servant-lucid
, servant-server, stdenv, text, time, transformers, wai, warp
}:
mkDerivation {
  pname = "mellon-server";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base either lucid mellon servant servant-docs servant-lucid
    servant-server text time transformers wai warp
  ];
  testHaskellDepends = [
    aeson base either hspec http-client http-types lucid mellon servant
    servant-docs servant-lucid servant-server text time transformers
    wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
