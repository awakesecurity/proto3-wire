{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
{   proto3-wire =
        pkgs.haskell.packages.${compiler}.callPackage ./default.nix { };
}
