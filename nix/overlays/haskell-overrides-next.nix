self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalMellon;
  inherit (haskell.lib) doJailbreak dontCheck;

in
{
  ## Testing with upcoming GHC releases.
}
