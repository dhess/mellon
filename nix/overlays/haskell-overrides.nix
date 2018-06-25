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

  withLts11Extras = hp: (hp.extend (self: super: (
    rec {
    }
  )));

in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages =
    withLocalMellon localMellonPathsAllTests (super.haskellPackages.extend (self: super:
      {
      }
    ));


  ## GHC 8.4.3. Note that we use hlint tests here.

  haskellPackages843 =
    withLocalMellon localMellonPathsAllTests (self.haskell.packages.ghc843.extend (self: super:
      {
        http-media = doJailbreak super.http-media;
        servant = doJailbreak super.servant;
        servant-client = doJailbreak super.servant-client;
        servant-server = doJailbreak super.servant-server;
        servant-swagger = super.callPackage ../pkgs/servant-swagger-ghc843.nix {};
        servant-swagger-ui = doJailbreak super.servant-swagger-ui;
      }
    ));


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.

  lts11Packages =
    withLocalMellon localMellonPaths (withLts11Extras self.haskell.packages.stackage.lts-1114);

}
