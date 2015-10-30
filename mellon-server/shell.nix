let
  pkgs = import <nixpkgs> {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      servant-client = if pkgs.stdenv.isDarwin then pkgs.haskell.lib.dontCheck super.servant-client else super.servant-client;
      mellon = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../mellon {}) "--ghc-options=-Werror");
      mellon-client = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../mellon-client {}) "--ghc-options=-Werror");
      mellon-pi = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../mellon-pi {}) "--ghc-options=-Werror");
      mellon-server = pkgs.haskell.lib.appendConfigureFlag (self.callPackage ./. {}) "--ghc-options=-Werror";
    };
  };

in haskellPackages.mellon-server.env
