## Build mellon in maintainer mode.

self: super:

let

  lib = (import ../lib);
  inherit (lib) haskell withLocalMellonMaintainer;
  inherit (haskell.lib) doJailbreak properExtend;

in
{
  ## The default Nixpkgs package set.
  haskellPackages =
    (withLocalMellonMaintainer (properExtend super.haskellPackages (self: super:
      {
        servant-docs = doJailbreak super.servant-docs;
        insert-ordered-containers = doJailbreak super.insert-ordered-containers;
        tdigest = doJailbreak super.tdigest;
      }
  )));
}
