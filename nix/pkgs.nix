import ./nixpkgs.nix {
  overlays = [
    (import ./haskell-packages.nix {
      compiler = "ghc884";
    })
  ];
}
