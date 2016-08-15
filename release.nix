let config = {
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskell.packages.ghc7103.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        proto3-wire = haskellPackagesOld.callPackage ./default.nix { };
      };
    };
  };
};
in

{ pkgs ? import <nixpkgs> { inherit config; } }:
{ proto3-wire = pkgs.haskellPackages.proto3-wire;
}
