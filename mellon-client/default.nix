{ mkDerivation, aeson, base, bytestring, doctest, either, hlint
, hspec, http-client, http-types, lucid, mellon-core, mellon-server
, network, servant, servant-client, stdenv, text, time
, transformers, wai, warp
}:
mkDerivation {
  pname = "mellon-client";
  version = "0.6.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring either http-client http-types lucid
    mellon-core mellon-server servant servant-client text time
    transformers
  ];
  testHaskellDepends = [
    aeson base bytestring doctest either hlint hspec http-client
    mellon-core mellon-server network servant servant-client text time
    transformers wai warp
  ];
  homepage = "https://github.com/dhess/mellon/";
  description = "Haskell client bindings for mellon-server";
  license = stdenv.lib.licenses.bsd3;
}
