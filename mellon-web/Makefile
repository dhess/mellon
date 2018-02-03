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

configure: nix mellon-web.cabal
	@echo "*** Configuring mellon-web"
	cabal configure -f test-hlint

nix: 	mellon-web.cabal
	@echo "*** Generating mellon-web Nix files"
	cd ../nix/pkgs && cabal2nix ../../mellon-web > mellon-web.nix
	cd ../nix/pkgs && cabal2nix --flag test-hlint ../../mellon-web > mellon-web-hlint.nix

mellon-web.cabal: package.yaml
	@echo "*** Running hpack for mellon-web"
	hpack

clean:
	cabal clean

.PHONY: clean nix mellon-web.cabal
