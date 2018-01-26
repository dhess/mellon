self: super:

let

  inherit (self) haskell;

in
{
  haskellPackages = super.haskellPackages.extend (self: super:
    with haskell.lib;
    rec {
      # Doesn't currently check.
      hpio = dontCheck super.hpio;

      mellon-core = self.callPackage ../pkgs/mellon-core.nix {};
      mellon-gpio = self.callPackage ../pkgs/mellon-gpio.nix {};
      mellon-web = self.callPackage ../pkgs/mellon-web.nix {};
    }
  );
}
