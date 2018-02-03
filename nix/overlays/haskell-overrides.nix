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

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages =
    withLocalMellon localMellonPathsHlint (super.haskellPackages.extend (self: super:
      {
        # Doesn't currently check.
        hpio = dontCheck super.hpio;
      }
    ));


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.

  lts10Packages =
    withLocalMellon localMellonPaths (self.haskell.packages.stackage.lts-104.extend (self: super:
      {
        # Doesn't currently check on macOS.
        foundation = dontCheck super.foundation;
      }
    ));

  # Don't waste time Haddock-ing these.

  lts9Packages =
    noHaddocks (withLocalMellon localMellonPaths (self.haskell.packages.stackage.lts-921.extend (self: super:
      {
        # Doesn't currently check on macOS.
        foundation = dontCheck super.foundation;

        # Doesn't check.
        zlib = dontCheck super.zlib;
      }
    )));

}
