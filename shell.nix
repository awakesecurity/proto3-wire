{ compiler ? "ghc982", enableStack ? false }:

let
  pkgs = import ./nix/pkgs.nix {
    inherit compiler;
  };

in pkgs.haskellPackages.shellFor {
  packages = hpkgs: [
    hpkgs.proto3-wire
  ];

  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.cabal2nix
  ] ++ (if enableStack then [ pkgs.stack ] else []);
}
