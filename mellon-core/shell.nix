{ compiler ? "ghc822" }:

(import ../release.nix { inherit compiler; }).mellon-core.env
