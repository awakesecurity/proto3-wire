args:

let
  nixpkgs = builtins.fetchTarball {
    # nixos-22.05 as on 2023-07-17
    url = "https://github.com/NixOS/nixpkgs/archive/380be19fbd2d9079f677978361792cb25e8a3635.tar.gz";
    sha256 = "154x9swf494mqwi4z8nbq2f0sp8pwp4fvx51lqzindjfbb9yxxv5";
  };
in import nixpkgs ({ config = { }; } // args)
