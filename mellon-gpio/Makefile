# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.

test:	build
	@echo "*** Running mellon-gpio tests"
	cabal test

build:	configure
	@echo "*** Building mellon-gpio"
	cabal build

doc:
	@echo "*** Generating mellon-gpio docs"
	cabal haddock --hyperlink-source

sdist:	check doc
	@echo "*** Creating mellon-gpio source distribution"
	cabal sdist

check:
	@echo "*** Checking mellon-gpio for errors"
	cabal check

configure: nix mellon-gpio.cabal
	@echo "*** Configuring mellon-gpio"
	cabal configure -f test-hlint

nix: 	mellon-gpio.cabal
	@echo "*** Generating mellon-gpio Nix files"
	cd ../nix/pkgs && cabal2nix ../../mellon-gpio > mellon-gpio.nix
	cd ../nix/pkgs && cabal2nix --flag test-hlint ../../mellon-gpio > mellon-gpio-maintainer.nix

clean:
	cabal clean

.PHONY: clean nix
