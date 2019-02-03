let

  localLib = import ./lib.nix;

in
[
  localLib.fetchHpio
  localLib.fetchNixPkgsLibQuixoftic
  ./overlays/lib.nix
  ./overlays/haskell-overrides.nix
]
