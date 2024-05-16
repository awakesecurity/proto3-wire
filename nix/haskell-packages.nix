{ compiler }:

pkgsNew: pkgsOld:

{
  haskellPackages = pkgsOld.haskell.packages."${compiler}".override (old: {
    overrides =
      pkgsNew.lib.fold pkgsNew.lib.composeExtensions
        (old.overrides or (_: _: {}))
        [ (pkgsNew.haskell.lib.packagesFromDirectory { directory = ./packages; })
          (haskellPackagesFinal: haskellPackagesPrev: {
            # With nixpkgs-23.11 and ghc981, adjunctions wants hspec for testing,
            # which causes problems.
            adjunctions =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.adjunctions;

            # With nixpkgs-23.11 and ghc981, aeson-2.1.2.1 thinks that th-abstraction is out of bounds.
            # Also, in order to avoid the breaking change to package structure in aeson-2.2.0.0,
            # we patch the import list of aeson-2.1.2.1.
            aeson =
              pkgsNew.haskell.lib.doJailbreak
                ( pkgsNew.haskell.lib.appendPatches haskellPackagesPrev.aeson
                    [ ./patches/aeson-2.1.2.1.patch ] );

            # With nixpkgs-23.11 and ghc981, atomic-write wants hspec for testing,
            # which causes problems.
            atomic-write =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.atomic-write;

            # With nixpkgs-23.11 and ghc981, base-compat-batteries wants hspec for testing,
            # which causes problems.
            base-compat-batteries =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.base-compat-batteries;

            # With nixpkgs-23.11 and ghc981, base-orphans wants hspec for testing,
            # which causes problems.
            base-orphans =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.base-orphans;

            # With nixpkgs-23.11 and ghc981, bifunctors wants hspec for testing,
            # which causes problems.
            bifunctors =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.bifunctors;

            # With nixpkgs-23.11 and ghc981, conduit wants hspec for testing,
            # which causes problems.
            conduit =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.conduit;

            # With nixpkgs-23.11 and ghc981, constraints wants hspec for testing,
            # which causes problems.
            constraints =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.constraints;

            # With nixpkgs-23.11 and ghc981, data-diverse wants hspec for testing,
            # which causes problems.
            data-diverse =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.data-diverse;

            # With nixpkgs-23.11 and ghc981, distribution-nixpkgs wants hspec for testing,
            # which causes problems.
            distribution-nixpkgs =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.distribution-nixpkgs;

            # With nixpkgs-23.11 and ghc981, distributive wants hspec for testing,
            # which causes problems.
            distributive =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.distributive;

            # With ghc981, doctest-0.22.2 complains about the version of the base
            # package and depends on hspec for testing, which causes problems.
            doctest =
              pkgsNew.haskell.lib.dontCheck
                (pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.doctest);

            # With nixpkgs-23.11 and ghc981, generic-deriving wants hspec for testing,
            # which causes problems.  Also, it generic-deriving thinks that
            # th-abstraction is out of bounds.
            generic-deriving =
              pkgsNew.haskell.lib.dontCheck
                (pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.generic-deriving);

            # With nixpkgs-23.11 and ghc981, half thinks that deepseq is out of bounds.
            half =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.half;

            # With nixpkgs-23.11 and ghc981, hourglass does not support the version
            # of the time package that is provided, but that matters only to tests.
            hourglass =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.hourglass;

            # With nixpkgs-23.11 and ghc981, hpack-0.36.0 wants hspec for testing,
            # which causes problems.
            hpack =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.hpack;

            # With nixpkgs-23.11 and ghc981, http-types wants hspec for testing,
            # which causes problems.
            http-types =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.http-types;

            # With nixpkgs-23.11 and ghc981, infer-license wants hspec for testing,
            # which causes problems.
            infer-license =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.infer-license;

            # With nixpkgs-23.11 and our overrides, insert-ordered-containers thinks that lens is out of bounds.
            insert-ordered-containers =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.insert-ordered-containers;

            # With nixpkgs-23.11 and ghc981, invariant indirectly depends on hspec for testing,
            # which causes problems. Also, it generic-deriving thinks that
            # th-abstraction is out of bounds.
            invariant =
              pkgsNew.haskell.lib.dontCheck
                (pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.invariant);

            # With nixpkgs-23.11 and ghc981, iproute wants hspec for testing,
            # which causes problems.
            iproute =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.iproute;

            # With nixpkgs-23.11 and ghc962, generics-sop-0.5.1.4 thinks that th-abstraction is out of bounds.
            generics-sop =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.generics-sop;

            # At a guess the test failures are some environmental issue,
            # like being unable to contact "download.fpcomplete.com" due
            # to the Nix build environment.
            http-download =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.http-download;

            # With nixpkgs-23.11 and ghc902, large-generics thinks that primitive is out of bounds.
            large-generics =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.large-generics;

            # With nixpkgs-23.11 and ghc902, large-records thinks that primitive is out of bounds.
            large-records =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.large-records;

            # With nixpkgs-23.11 and ghc981 (or perhaps our customized dependencies),
            # the tests in lifted-base fail.
            lifted-base =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.lifted-base;

            # With nixpkgs-23.11 and our overrides, microlens-th thinks that th-abstraction is out of bounds.
            microlens-th =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.microlens-th;

            # With nixpkgs-23.11 and ghc981, monad-par wants test-framework for testing, which
            # wants language-haskell-extract, which does not support modern template-haskell.
            monad-par =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.monad-par;

            # With nixpkgs-23.11 and ghc981, mono-traversable wants hspec for testing,
            # which causes problems.
            mono-traversable =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.mono-traversable;

            # With nixpkgs-23.11 and our overrides, neat-interpolation that rebase is out of bounds.
            neat-interpolation =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.neat-interpolation;

            # Patch to enable building of pantry when using ghc902.
            #
            # Also disable failing tests.  At a guess the test failures are some
            # environmental issue, like being unable to contact "hackage.haskell.org"
            # due to the Nix build environment.
            pantry_0_9_2 =
              pkgsNew.haskell.lib.dontCheck
                ( pkgsNew.haskell.lib.appendPatches haskellPackagesPrev.pantry_0_9_2
                    [ ./patches/pantry-0.9.3.2.patch ] );

            # With nixpkgs-23.11 and our overrides, rerebase that rebase is out of bounds.
            rerebase =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.rerebase;

            # With nixpkgs-23.11 and ghc981, safe-exceptions wants hspec for testing,
            # which causes problems.
            safe-exceptions =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.safe-exceptions;

            # With nixpkgs-23.11 and ghc981, streaming-commons wants hspec for testing,
            # which causes problems.
            streaming-commons =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.streaming-commons;

            # With nixpkgs-23.11 and our dependency overrides, swagger2 wants hspec for testing,
            # which causes problems. Also, we jailbreak to allow a newer version of lens.
            swagger2 =
              pkgsNew.haskell.lib.dontCheck
                (pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.swagger2);

            # With nixpkgs-23.11 and ghc981, reflection indirectly depends on hspec for testing,
            # which causes problems.
            reflection =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.reflection;

            # With nixpkgs-23.11 and ghc981, resourceat wants hspec for testing,
            # which causes problems.
            resourceat =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.resourceat;

            # With nixpkgs-23.11 and our overrides, stack objects to a number of dependencies,
            # at least on ghc902.
            stack =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.stack;

            # With nixpkgs-23.11 and ghc981, resourcet wants hspec for testing,
            # which causes problems.
            resourcet =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.resourcet;

            # At a guess the test build failures are a bytestring version issue.
            tar-conduit =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.tar-conduit;

            # With nixpkgs-23.11 and ghc981, tasty-discover wants hspec for testing,
            # which causes problems.
            tasty-discover =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.tasty-discover;

            # Suppress:
            #   warning: non-portable path to file '"dist/build/Test/autogen/cabal_macros.h"'; specified path differs in case from file name on disk [-Wnonportable-include-path]
            tasty-golden =
              pkgsNew.haskell.lib.appendConfigureFlags haskellPackagesPrev.tasty-golden
                [ "--ghc-option=-Wno-nonportable-include-path" ];

            # With nixpkgs-23.11 and ghc981, text-metrics wants hspec for testing,
            # which causes problems.
            text-metrics =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.text-metrics;

            # With nixpkgs-23.11 and ghc981, th-compat wants hspec for testing,
            # which causes problems.
            th-compat =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.th-compat;

            # With nixpkgs-23.11 and our overrides, th-expand-syns thinks that th-abstraction is out of bounds.
            th-expand-syns =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.th-expand-syns;

            # With nixpkgs-23.11 and our overrides, th-lift thinks that th-abstraction is out of bounds.
            th-lift =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.th-lift;

            # With nixpkgs-23.11 and ghc981, unix-compat wants hspec for testing,
            # which causes problems.
            unix-compat =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.unix-compat;

            # With nixpkgs-23.11 and ghc981, hpack-0.36.0 wants hspec for testing,
            # which causes problems.
            unix-time =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.unix-time;

            # With nixpkgs-23.11 and ghc981, yaml wants hspec for testing,
            # which causes problems.
            yaml =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.yaml;
          })
          (haskellPackagesFinal: haskellPackagesPrev: {
            proto3-wire = haskellPackagesFinal.callCabal2nix "proto3-wire" ../. { };
          })
        ];
  });
}
