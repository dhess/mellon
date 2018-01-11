# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.

test:	build
	@echo "*** Running mellon-core tests"
	cabal test

build:	configure
	@echo "*** Building mellon-core"
	cabal build

doc:
	@echo "*** Generating mellon-core docs"
	cabal haddock --hyperlink-source

sdist:	check doc
	@echo "*** Creating mellon-core source distribution"
	cabal sdist

check:
	@echo "*** Checking mellon-core for errors"
	cabal check

configure: mellon-core.cabal ../nix/mellon-core.nix
	@echo "*** Configuring mellon-core"
	cabal configure

../nix/mellon-core.nix: mellon-core.cabal
	@echo "*** Generating ../nix/mellon-core.nix"
	cd ../nix && cabal2nix ../mellon-core > mellon-core.nix

mellon-core.cabal: package.yaml
	@echo "*** Running hpack for mellon-core"
	hpack

clean:
	cabal clean

.PHONY: clean
