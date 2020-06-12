{ compiler ? "ghc865" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev    = "d2a2ec2ebe49c42127cbf316d215a64c60d68fde";
    sha256 = "09p9cr07frsqh7vip2i7cp86xnafg1pxhbnphx0q4sd5bvilqpfm";
  };

  config   = { allowUnfree = true; };

  upgrade = packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskellPackagesNew.callPackage (./nix + "/${name}.nix") { };
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  dontCheck = haskell: packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskell.lib.dontCheck haskellPackagesOld.${name};
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  jailbreak = haskell: packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskell.lib.doJailbreak haskellPackagesOld.${name};
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  patch = haskell: packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskell.lib.appendPatch haskellPackagesOld.${name}
                                          (./nix + "/${name}.patch");
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  composeExtensionList = lib: lib.foldr lib.composeExtensions (_: _: {});

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

                upgradeOverrides = upgrade
                  [ "parameterized"
                  ];

                patchOverrides = patch haskell
                  [ "parameterized"
                  ];

                dontCheckOverrides = dontCheck haskell
                  [ # Add package name strings here.
                  ];

                jailbreakOverrides = jailbreak haskell
                  [ # Add package name strings here.
                  ];

                manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                };

              in
                composeExtensionList newPkgs.lib
                  [ packageSourceOverrides
                    upgradeOverrides
                    patchOverrides
                    dontCheckOverrides
                    jailbreakOverrides
                    manualOverrides
                  ];
          };
        };
      };
    })
  ];

  pkgs = import nixpkgs { inherit config overlays; };

in

  { proto3-wire = pkgs.haskell.packages."${compiler}".proto3-wire; }
