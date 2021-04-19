{ compiler ? "ghc884" }:

let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/9a1672105db0eebe8ef59f310397435f2d0298d0.tar.gz";
    sha256 = "06z4r0aaha5qyd0prg7h1f5sjrsndca75150zf5w4ff6g9vdv1rb";
  };

  config = { allowBroken = true; };

  overlay =
    pkgsNew: pkgsOld: rec {

      haskell = pkgsOld.haskell // {
        packages = pkgsOld.haskell.packages // {
          "${compiler}" = pkgsOld.haskell.packages.${compiler}.override (old: {
            overrides =
              let
                packageSourceOverrides = pkgsNew.haskell.lib.packageSourceOverrides {
                  "proto3-wire" = ./.;
                };

                manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                  parameterized =
                    pkgsNew.haskell.lib.dontCheck
                      haskellPackagesOld.parameterized;
                };

              in
                pkgsNew.lib.foldr pkgsNew.lib.composeExtensions (old.overrides or (_: _: { }))
                  [ packageSourceOverrides
                    manualOverrides
                  ];
          });
        };
      };
    };

  pkgs = import nixpkgs { inherit config; overlays = [ overlay ]; };

in
  { proto3-wire = pkgs.haskell.packages."${compiler}".proto3-wire;
  }
