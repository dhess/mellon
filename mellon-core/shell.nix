let
  pkgs = import <nixpkgs> {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      servant-client = if pkgs.stdenv.isDarwin then pkgs.haskell.lib.dontCheck super.servant-client else super.servant-client;
      mellon-web = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../mellon-web {}) "--ghc-options=-Werror");
      mellon-gpio = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../mellon-gpio {}) "--ghc-options=-Werror");
      mellon-core = pkgs.haskell.lib.appendConfigureFlag (self.callPackage ./. {}) "--ghc-options=-Werror";
    };
  };

in haskellPackages.mellon-core.env
