self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalMellon;
  inherit (haskell.lib) doJailbreak dontCheck noHaddocks properExtend;

in
{
  haskellPackages =
    withLocalMellon (properExtend super.haskellPackages (self: super:
      {
        servant-docs = doJailbreak super.servant-docs;
        insert-ordered-containers = doJailbreak super.insert-ordered-containers;
        tdigest = doJailbreak super.tdigest;
      }
    ));
}
