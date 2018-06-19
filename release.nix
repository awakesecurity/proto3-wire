{ compiler ? "ghc822" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev          = "e942479be49c3d1b586201be7488d7190ff46255";
    sha256       = "0q4jdlvx0i1bhxy7xk85mflv7fchlap28sis9qn4p3naz2l46mbw";
    outputSha256 = "0n424nkkjzvvsk0bcvb4lhyjl4k88nc5dzayjvscsqxssh1g06yh";
  };

  config = {
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides =
              let
                packageSourceOverrides = pkgs.haskell.lib.packageSourceOverrides {
                  "proto3-wire" = ./.;
                };

                manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                };

              in
                pkgs.lib.composeExtensions
                  packageSourceOverrides
                  manualOverrides;
          };
        };
      };
    };
  };

  pkgs = import nixpkgs { inherit config; };

in

{ proto3-wire = pkgs.haskell.packages."${compiler}".proto3-wire;
}
