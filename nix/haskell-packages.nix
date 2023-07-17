{ compiler }:

pkgsNew: pkgsOld:

{
  haskellPackages = pkgsOld.haskell.packages."${compiler}".override (old: {
    overrides =
      pkgsOld.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (haskellPackagesFinal: haskellPackagesPrev: {
          proto3-wire = haskellPackagesFinal.callCabal2nix "proto3-wire" ../. { };

          # Use newer version 4.7.1.0 for ghc-9.x support
          data-diverse = haskellPackagesFinal.callPackage ./data-diverse.nix { };
        });
  });
}
