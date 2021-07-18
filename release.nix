let
  pkgs = import ./nix/pkgs.nix;

in {
  proto3-wire = pkgs.haskell.lib.buildStrictly pkgs.haskellPackages.proto3-wire;
}
