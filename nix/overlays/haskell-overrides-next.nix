self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalMellon;
  inherit (haskell.lib) dontCheck noHaddocks;

  localMellonPathsAllTests = {
    mellon-core = ../pkgs/mellon-core-all-tests.nix;
    mellon-gpio = ../pkgs/mellon-gpio-all-tests.nix;
    mellon-web = ../pkgs/mellon-web-all-tests.nix;
  };

  localMellonPaths = {
    mellon-core = ../pkgs/mellon-core.nix;
    mellon-gpio = ../pkgs/mellon-gpio.nix;
    mellon-web = ../pkgs/mellon-web.nix;
  };

in
{

  ## Testing with upcoming GHC releases. Don't bother Haddock-ing
  ## these as they're unlikely to be cached by upstream Hydra.

  haskellPackages841 =
    noHaddocks (withLocalMellon localMellonPathsAllTests (self.haskell.packages.ghc841.extend (self: super:
      {
        # Doesn't currently check.
        hpio = dontCheck super.hpio;
      }
    )));

}
