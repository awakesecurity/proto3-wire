{ compiler }:

import ./nixpkgs.nix {
  overlays = [
    (import ./haskell-packages.nix {
      inherit compiler;
    })
  ];
}
