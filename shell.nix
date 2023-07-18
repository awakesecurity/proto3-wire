{ compiler ? "ghc8107" }:

let
  pkgs = import ./nix/pkgs.nix {
    inherit compiler;
  };

  proto3-wire = pkgs.haskellPackages.proto3-wire;

in proto3-wire.env.overrideAttrs (old: {
  buildInputs = (old.buildInputs or []) ++ [
    pkgs.cabal-install
    pkgs.stack
  ];
})
