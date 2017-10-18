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

sdist:	check
	@echo "*** Creating mellon-gpio source distribution"
	cabal sdist

check:
	@echo "*** Checking mellon-gpio for errors"
	cabal check

configure: mellon-gpio.cabal
	@echo "*** Configuring mellon-gpio"
	cabal configure

#mellon-gpio.cabal: package.yaml
#	@echo "*** Running hpack"
#	hpack

clean:
	cabal clean

.PHONY: clean
