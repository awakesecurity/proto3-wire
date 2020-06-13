{ compiler ? "ghc865" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgsRelease = "20.03";
  unpatchedNixpkgs = fetchNixpkgs {
    rev    = "fdfd5ab05444c38a006cb107d7d1ee8cb0b15719";
    sha256 = "17hsjpjahl0hff3z2khrcwxygjyyrav2pia3qqlli0sgywfrgf95";
  };

  config = { allowUnfree = true; };

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
          "${compiler}" = oldPkgs.haskell.packages.${compiler}.override {
            overrides =
              let
                packageSourceOverrides = oldPkgs.haskell.lib.packageSourceOverrides {
                  "proto3-wire" = ./.;
                };

                upgradeOverrides = upgrade
                  [ "ChasingBottoms"
                    "comonad"
                    "distributive"
                    "doctest"
                    "parameterized"
                    "semigroupoids"
                  ];

                patchOverrides = patch haskell
                  [ "parameterized"
                  ];

                dontCheckOverrides = dontCheck haskell
                  [ "doctest"
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

  unpatchedPkgs = import unpatchedNixpkgs { inherit config overlays; };

  # https://github.com/NixOS/nixpkgs/pull/85446
  nixpkgs = unpatchedPkgs.stdenvNoCC.mkDerivation {
    name = "nixpkgs-${nixpkgsRelease}-patched";

    src = unpatchedNixpkgs;

    # Backport fix <https://github.com/NixOS/nixpkgs/pull/85446> to 20.03:
    patches = [ ./nix/with-packages-wrapper.patch ];

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];

    installPhase = ''
      mkdir -p $out
      cp -R ./ $out/
    '';

    preferLocalBuild = true;
  };

  pkgs = import nixpkgs { inherit config overlays; };

in

  { proto3-wire = pkgs.haskell.packages."${compiler}".proto3-wire; }
