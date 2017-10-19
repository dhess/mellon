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

configure: mellon-core.cabal
	@echo "*** Configuring mellon-core"
	cabal configure

mellon-core.cabal: package.yaml
	@echo "*** Running hpack for mellon-core"
	hpack

clean:
	cabal clean

.PHONY: clean
