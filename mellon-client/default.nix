{ mkDerivation, aeson, base, bytestring, either, hspec, http-client
, http-types, lucid, mellon, mellon-server, network, servant
, servant-client, stdenv, text, time, transformers, wai, warp
}:
mkDerivation {
  pname = "mellon-client";
  version = "0.6.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring either http-client http-types lucid mellon
    mellon-server servant servant-client text time transformers
  ];
  testHaskellDepends = [
    aeson base bytestring either hspec http-client mellon mellon-server
    network servant servant-client text time transformers wai warp
  ];
  description = "Haskell client bindings for mellon-server";
  license = stdenv.lib.licenses.bsd3;
}
