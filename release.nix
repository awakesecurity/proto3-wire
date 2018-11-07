{ compiler ? "ghc822" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
   rev          = "a8ff2616603a6ff6bfb060368c12a973c8e007f6";
   sha256       = "15l57ra62w9imqv3cfx9qp1fag3mqp95x0hdh81cqjb663qxihlg";
   outputSha256 = "1nkpbwdx1jgr2pv5arllk6k56h3xc61jal7qi66g21qsx6daf0g3";
  };

  config   = { allowUnfree = true; };

  overlays = [
    (newPkgs: oldPkgs: rec {

      haskell = oldPkgs.haskell // {
        packages = oldPkgs.haskell.packages // {
          "${compiler}" = oldPkgs.haskell.packages."${compiler}".override {
            overrides =
              let
                packageSourceOverrides = oldPkgs.haskell.lib.packageSourceOverrides {
                  "proto3-wire" = ./.;
                };

                manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                };

              in
                newPkgs.lib.composeExtensions
                  packageSourceOverrides
                  manualOverrides;
          };
        };
      };
    })
  ];

  pkgs = import nixpkgs { inherit config overlays; };

in

  { proto3-wire = pkgs.haskell.packages."${compiler}".proto3-wire; }
