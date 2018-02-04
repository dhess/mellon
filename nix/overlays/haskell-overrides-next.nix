self: super:

let

  inherit (self) haskell withLocalMellon;
  inherit (haskell.lib) dontCheck noHaddocks;

  localMellonPathsHlint = {
    mellon-core = ../pkgs/mellon-core-hlint.nix;
    mellon-gpio = ../pkgs/mellon-gpio-hlint.nix;
    mellon-web = ../pkgs/mellon-web-hlint.nix;
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
    noHaddocks (withLocalMellon localMellonPaths (self.haskell.packages.ghc841.extend (self: super:
      {
        # Doesn't currently check.
        hpio = dontCheck super.hpio;
      }
    )));

}
