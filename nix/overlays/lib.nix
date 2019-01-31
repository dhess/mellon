self: super:

let

  inherit (self) lib;
  inherit (self.haskell.lib) properExtend;


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

  withLocalMellon = hp: (properExtend hp (self: super: (
    {
      mellon-core = myCleanPackage (super.callPackage ../pkgs/mellon-core.nix {});
      mellon-gpio = myCleanPackage (super.callPackage ../pkgs/mellon-gpio.nix {});
      mellon-web = myCleanPackage (super.callPackage ../pkgs/mellon-web.nix {});

      mellon-core-all-tests = myCleanPackage (super.callPackage ../pkgs/mellon-core-all-tests.nix {});
      mellon-gpio-all-tests = myCleanPackage (super.callPackage ../pkgs/mellon-gpio-all-tests.nix {});
      mellon-web-all-tests = myCleanPackage (super.callPackage ../pkgs/mellon-web-all-tests.nix {});
    }
  )));

in
{
  lib = (super.lib or {}) // {

    inherit withLocalMellon;

    maintainers = super.lib.maintainers // {
      dhess-qx = "Drew Hess <src@drewhess.com>";
    };

  };
}
