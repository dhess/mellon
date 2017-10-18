{ compiler ? "ghc821" }:

(import ./release.nix { inherit compiler; }).mellon-core.env
