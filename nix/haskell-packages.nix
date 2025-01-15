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

            # With nixpkgs-24.11 and our overrides, lens thinks that template-haskell is out of bounds.
            lens =
              pkgsNew.haskell.lib.doJailbreak haskellPackagesPrev.lens;

            # With nixpkgs-23.11 and ghc981, monad-par wants test-framework for testing, which
            # wants language-haskell-extract, which does not support modern template-haskell.
            monad-par =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.monad-par;

            # With nixpkgs-23.11 and ghc981, mono-traversable wants hspec for testing,
            # which causes problems.
            mono-traversable =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.mono-traversable;

            # With nixpkgs-23.11 and ghc981, streaming-commons wants hspec for testing,
            # which causes problems.
            streaming-commons =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.streaming-commons;

            # With nixpkgs-23.11 and ghc981, reflection indirectly depends on hspec for testing,
            # which causes problems.
            reflection =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.reflection;

            # With nixpkgs-23.11 and ghc981, resourcet wants hspec for testing,
            # which causes problems.
            resourcet =
              pkgsNew.haskell.lib.dontCheck haskellPackagesPrev.resourcet;

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
