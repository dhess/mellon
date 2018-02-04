# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.

# Implementation notes:
#
# The "test" and "build" targets simply build mellon-web, as it
# depends on both of the other packages. Testing/building just this
# package ensures that everything is built only once under Nix,
# whereas building each package separately causes each dependent
# "mellon" package to be built (and tested) again, needlessly.


SUBPROJECTS = mellon-core \
	      mellon-gpio \
	      mellon-web

NIXPKGS := $(shell nix-build -Q --no-out-link ./nix/fetch-nixpkgs-stackage-nixpkgs.nix 2>/dev/null)

nix-build-testing-attr = nix-build --no-out-link nix/jobsets/testing.nix -I nixpkgs=$(NIXPKGS) -A $(1)

nix-build-testing = nix-build --no-out-link nix/jobsets/testing.nix -I nixpkgs=$(NIXPKGS)

nix-build-attr = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS) -A $(1)

nix-build = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS)

mellon:	nix
	$(call nix-build-testing)

mellon-%:	nix
		$(call nix-build-testing-attr,mellon-$*)

nixpkgs:	nix
		$(call nix-build-attr,nixpkgs)

lts-%:	nix
	$(call nix-build-attr,lts-$*)

release:	nix
		$(call nix-build)

# Note: does not depend on nixpkgs.
next:	nix
	nix-build --no-out-link nix/jobsets/next.nix

test:	build
	$(MAKE) -C mellon-web test

build:	nix
	$(MAKE) -C mellon-web build

help:
	@echo "Targets:"
	@echo
	@echo "(Default is 'nix-build')"
	@echo
	@echo "Cabal/Nix:"
	@echo
	@echo "The following targets assume that you are running Nix with some version"
	@echo "of cabal and GHC in your environment."
	@echo
	@echo "    nix-build - Build all nix/jobsets/release.nix attrs"
	@echo "    test      - configure and build the package, then run the tests"
	@echo "    build     - configure and build the package"
	@echo "    nix       - make sure Nix files are up-to-date"
	@echo "    doc	     - build docs"
	@echo "    sdist     - build source distributions for upload to Hackage"
	@echo "    check     - run 'cabal check' on each subproject"
	@echo
	@echo "General:"
	@echo
	@echo "    clean - remove all targets"
	@echo "    help  - show this message"

nix:
	@for proj in $(SUBPROJECTS); do \
	  $(MAKE) -C $$proj nix; \
	done

doc:
	@for proj in $(SUBPROJECTS); do \
	  $(MAKE) -C $$proj doc; \
	done

sdist:	check doc
	@for proj in $(SUBPROJECTS); do \
	  $(MAKE) -C $$proj sdist; \
	done

check:
	@for proj in $(SUBPROJECTS); do \
	  $(MAKE) -C $$proj check; \
	done

clean:
	@for proj in $(SUBPROJECTS); do \
	  $(MAKE) -C $$proj clean; \
	done

nix-stack = nix-shell -p stack-env zlib libiconv ncurses --run 'stack test --stack-yaml $(1)'

stack-lts:      stack-lts-10

stack-lts-%:    nix
		$(call nix-stack, stack-lts-$*.yaml)

.PHONY: clean nix nix-build
