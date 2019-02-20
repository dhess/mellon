let

  lib = (import nix/lib);
  defaultPkgs = lib.nixpkgs {};

in

{ pkgs ? defaultPkgs }:

let

  mellonOverlays = self: super:
    lib.customisation.composeOverlays lib.overlays super;
  mellonOverlaysMaintainer = self: super:
    lib.customisation.composeOverlays lib.maintainerOverlays super;

  mellonCoreNix = nix/pkgs/mellon-core.nix;
  mellonCoreNixMaintainer = nix/pkgs/mellon-core-maintainer.nix;

  mellonGpioNix = nix/pkgs/mellon-gpio.nix;
  mellonGpioNixMaintainer = nix/pkgs/mellon-gpio-maintainer.nix;

  mellonWebNix = nix/pkgs/mellon-web.nix;
  mellonWebNixMaintainer = nix/pkgs/mellon-web-maintainer.nix;

  mellonPkgs = lib.customisation.composeOverlays (lib.singleton mellonOverlays) pkgs;
  mellonPkgsMaintainer = lib.customisation.composeOverlays (lib.singleton mellonOverlaysMaintainer) pkgs;

in
{
  # haskellPackages with the local mellon packages. Note that this
  # package set builds mellon *without* maintainer tests.
  inherit (mellonPkgs) haskellPackages;

  # The path to the local mellon Nix files, in case you want to make
  # your own.
  inherit mellonCoreNix mellonGpioNix mellonWebNix;

  # Same as the above, except with the mellon packages in maintainer
  # mode.
  maintainer = {
    inherit (mellonPkgsMaintainer) haskellPackages;
    mellonCoreNix = mellonCoreNixMaintainer;
    mellonGpioNix = mellonGpioNixMaintainer;
    mellonWebNix = mellonWebNixMaintainer;
  };

  overlays.mellon = mellonOverlays;
  overlays.mellonMaintainer = mellonOverlaysMaintainer;
}
