self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalMellon;
  inherit (haskell.lib) doJailbreak dontCheck noHaddocks;

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

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages =
    withLocalMellon localMellonPathsAllTests (super.haskellPackages.extend (self: super:
      {
      }
    ));


  ## GHC 8.4.1. Note that we use hlint tests here.

  haskellPackages841 =
    withLocalMellon localMellonPathsAllTests (self.haskell.packages.ghc841.extend (self: super:
      {
        http-media = doJailbreak super.http-media;
        servant = doJailbreak super.servant;
        servant-client = doJailbreak super.servant-client;
        servant-server = doJailbreak super.servant-server;
        servant-swagger = doJailbreak super.servant-swagger;
        servant-swagger-ui = doJailbreak super.servant-swagger-ui;
        swagger2 = super.callPackage ../pkgs/swagger2-2.2.1.nix {};
      }
    ));


  # Currently, armv7l-linux on Nixpkgs must use ghc802.

  haskellPackagesArmv7l =
    withLocalMellon localMellonPaths (self.haskell.packages.ghc802.extend (self: super:
      {
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
        protolude = self.callPackage ../pkgs/protolude-0.2.nix {};

        # Doesn't currently check on macOS.
        foundation = dontCheck super.foundation;

        # Doesn't check.
        zlib = dontCheck super.zlib;
      }
    )));

}
