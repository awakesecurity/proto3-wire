{ compiler ? "ghc865" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev          = "d2a2ec2ebe49c42127cbf316d215a64c60d68fde";
    outputSha256 = "09p9cr07frsqh7vip2i7cp86xnafg1pxhbnphx0q4sd5bvilqpfm";
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
