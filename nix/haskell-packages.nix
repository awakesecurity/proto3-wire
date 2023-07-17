{ compiler }:

pkgsNew: pkgsOld:

{
  haskellPackages = pkgsOld.haskell.packages."${compiler}".override (old: {
    overrides =
      pkgsOld.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (haskellPackagesFinal: haskellPackagesPrev: {
          proto3-wire = haskellPackagesFinal.callCabal2nix "proto3-wire" ../. { };

          # ghc-9.2 requires word-compat-0.0.6
          word-compat = haskellPackagesFinal.callPackage ./word-compat.nix { };
          # Use newer version 4.7.1.0 for ghc-9.x support
          data-diverse = haskellPackagesFinal.callPackage ./data-diverse.nix { };
        });
  });
}
