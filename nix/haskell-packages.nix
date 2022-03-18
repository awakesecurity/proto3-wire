{ compiler }:

pkgsNew: pkgsOld:

{
  haskellPackages = pkgsOld.haskell.packages."${compiler}".override (old: {
    overrides =
      pkgsOld.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (haskellPackagesFinal: haskellPackagesPrev: {
          proto3-wire = haskellPackagesPrev.callCabal2nix "proto3-wire" ../. { };
        });
  });
}
