self: super:

let

  inherit (self) lib;


  ## Ignore local files that shouldn't contribute to the Nix hash.
  ## Ideally this would be based on the cabal sdist contents, but
  ## that's not easily do-able at the moment.

  filterSourceLocal = name: type: let baseName = baseNameOf (toString name); in ! (
    type == "directory" && (
      baseName == "scripts"
    ) ||
    type != "directory" && (
      baseName == "Makefile"      ||
      baseName == "source.txt"    ||
      baseName == "cabal.project"
    )
  );
  cleanSourceLocal = src: lib.cleanSourceWith { filter = filterSourceLocal; inherit src; };

  myCleanSource = src: cleanSourceLocal (lib.cleanSourceAllExtraneous src);
  myCleanPackage = pkg: lib.cleanPackage myCleanSource pkg;


  ## Haskell package combinators.

  withLocalMellon = localMellonPaths: hp: (hp.extend (self: super: (
    {
      mellon-core = myCleanPackage (super.callPackage localMellonPaths.mellon-core {});
      mellon-gpio = myCleanPackage (super.callPackage localMellonPaths.mellon-gpio {});
      mellon-web = myCleanPackage (super.callPackage localMellonPaths.mellon-web {});
    }
  )));

in
{
  inherit withLocalMellon;
}
