let
  pkgs = import ./nix/pkgs.nix;

in {
  proto3-wire = pkgs.haskell.lib.buildFromSdist pkgs.haskellPackages.proto3-wire;
}
