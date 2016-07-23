let config = {
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        proto3-wire = haskellPackagesOld.callPackage ./default.nix { };
      };
    };
  };
};
in

{ pkgs ? import <nixpkgs> { inherit config; } }:
pkgs.haskellPackages.proto3-wire
