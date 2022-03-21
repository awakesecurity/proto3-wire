{ compiler }:

pkgsNew: pkgsOld:

{
  haskellPackages = pkgsOld.haskell.packages."${compiler}".override (old: {
    overrides =
      pkgsOld.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (haskellPackagesFinal: haskellPackagesPrev: {
          proto3-wire = haskellPackagesFinal.callCabal2nix "proto3-wire" ../. { };
          word-compat = haskellPackagesFinal.callPackage ./word-compat.nix { };
        });
  });
}
