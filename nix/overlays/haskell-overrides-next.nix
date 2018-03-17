self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalMellon;
  inherit (haskell.lib) doJailbreak dontCheck;

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

  ## Testing with upcoming GHC releases.

  haskellPackages841 =
    withLocalMellon localMellonPathsAllTests (self.haskell.packages.ghc841.extend (self: super:
      {
        http-media = doJailbreak super.http-media;
        servant = doJailbreak super.servant;
        swagger2 = doJailbreak super.swagger2;
      }
    ));

}
