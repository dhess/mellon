{ mkDerivation, aeson, base, bytestring, either, hspec, http-types
, lucid, mellon, mellon-server, servant, servant-client, stdenv
, text, time, transformers, wai, warp
}:
mkDerivation {
  pname = "mellon-client";
  version = "0.3.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring either http-types lucid mellon mellon-server
    servant servant-client text time transformers
  ];
  testHaskellDepends = [
    aeson base bytestring either hspec mellon mellon-server servant
    servant-client text time transformers wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
