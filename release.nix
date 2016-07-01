{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
{   proto3-wire =
        nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./default.nix { };
}
