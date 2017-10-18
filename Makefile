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

NIXFILES = nix/mellon-core.nix \
	   nix/mellon-gpio.nix \
	   nix/mellon-web.nix 

test:	$(NIXFILES)
	$(MAKE) -C mellon-web configure
	$(MAKE) -C mellon-web test

help:
	@echo "Targets:"
	@echo
	@echo "Cabal/Nix:"
	@echo
	@echo "The following targets assume that you are running Nix with some version"
	@echo "of cabal and GHC in your environment."
	@echo
	@echo "    test      - configure and build the package, then run the tests"
	@echo "    build     - configure and build the package"
	@echo
	@echo "Stack/Nix:"
	@echo
	@echo "The following targets build and test the package with Stack, using the"
	@echo "given version of Stackage LTS as configured by the file stack-<target>.yaml."
	@echo
	@echo "    lts   [build all supported LTS targets]"
	@echo "    lts-9"
	@echo "    lts-6"
	@echo "    lts-3"
	@echo "    lts-2 [Note: does not work on macOS]"
	@echo
	@echo "General:"
	@echo
	@echo "    clean - remove all targets"
	@echo "    help  - show this message"

build:	$(NIXFILES)
	$(MAKE) -C mellon-web configure
	$(MAKE) -C mellon-web build

sdist:	check
	@for proj in $(SUBPROJECTS); do \
	  $(MAKE) -C $$proj sdist; \
	done

check:
	@for proj in $(SUBPROJECTS); do \
	  $(MAKE) -C $$proj check; \
	done

nix-stack = nix-shell -p stack-$(1)-env --run 'stack test --nix --nix-packages "zlib binutils gcc" --stack-yaml $(2)'

lts:	lts-9 lts-6 lts-3 lts-2

lts-9: 	$(NIXFILES)
	$(call nix-stack,lts-9,stack.yaml)

# Currently disabled, as Nix no longer supports GHC 8.0.1 out of the
# box.

#lts-7: 	mellon-web.cabal nix/mellon-web.nix
#	$(call nix-stack,lts-7,stack-lts-7.yaml)

lts-6: 	$(NIXFILES)
	$(call nix-stack,lts-6,stack-lts-6.yaml)

lts-3: 	$(NIXFILES)
	$(call nix-stack,lts-3,stack-lts-3.yaml)

lts-2: 	$(NIXFILES)
	$(call nix-stack,lts-2,stack-lts-2.yaml)

nix/mellon-core.nix: mellon-core/mellon-core.cabal
	@echo "*** Generating nix/mellon-core.nix"
	cd nix && cabal2nix ../mellon-core > mellon-core.nix

nix/mellon-gpio.nix: mellon-gpio/mellon-gpio.cabal
	@echo "*** Generating nix/mellon-gpio.nix"
	cd nix && cabal2nix ../mellon-gpio > mellon-gpio.nix

nix/mellon-web.nix: mellon-web/mellon-web.cabal
	@echo "*** Generating nix/mellon-web.nix"
	cd nix && cabal2nix ../mellon-web > mellon-web.nix

clean:
	@for proj in $(SUBPROJECTS); do \
	  $(MAKE) -C $$proj clean; \
	done

.PHONY: clean
