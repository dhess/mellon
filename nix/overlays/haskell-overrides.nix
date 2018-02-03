self: super:

let

  inherit (self) haskell withLocalMellon;
  inherit (haskell.lib) dontCheck;

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
  haskellPackages = withLocalMellon localMellonPathsHlint (super.haskellPackages.extend (self: super:
    {
      # Doesn't currently check.
      hpio = dontCheck super.hpio;
    }
  ));
}
