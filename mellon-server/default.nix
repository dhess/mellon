{ mkDerivation, aeson, base, bytestring, doctest, either, hlint
, hspec, hspec-wai, http-client, http-types, lucid, mellon-core
, servant, servant-docs, servant-lucid, servant-server, stdenv
, text, time, transformers, wai, wai-extra, warp
}:
mkDerivation {
  pname = "mellon-server";
  version = "0.6.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring either http-types lucid mellon-core servant
    servant-docs servant-lucid servant-server text time transformers
    wai warp
  ];
  executableHaskellDepends = [ base mellon-core warp ];
  testHaskellDepends = [
    aeson base bytestring doctest either hlint hspec hspec-wai
    http-client http-types lucid mellon-core servant servant-docs
    servant-lucid servant-server text time transformers wai wai-extra
    warp
  ];
  homepage = "https://github.com/dhess/mellon/";
  description = "A REST web service for mellon";
  license = stdenv.lib.licenses.bsd3;
}
