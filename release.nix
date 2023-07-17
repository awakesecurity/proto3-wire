{ compiler ? "ghc8107" }:

let
  pkgs = import ./nix/pkgs.nix {
    inherit compiler;
  };
in {
  proto3-wire = pkgs.haskell.lib.buildStrictly pkgs.haskellPackages.proto3-wire;
}
