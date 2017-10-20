# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.

test:	build
	@echo "*** Running mellon-web tests"
	cabal test

build:	configure
	@echo "*** Building mellon-web"
	cabal build

doc:
	@echo "*** Generating mellon-web docs"
	cabal haddock --hyperlink-source

sdist:	check doc
	@echo "*** Creating mellon-web source distribution"
	cabal sdist

check:
	@echo "*** Checking mellon-web for errors"
	cabal check

configure: mellon-web.cabal
	@echo "*** Configuring mellon-web"
	cabal configure

mellon-web.cabal: package.yaml
	@echo "*** Running hpack for mellon-web"
	hpack

clean:
	cabal clean

.PHONY: clean
