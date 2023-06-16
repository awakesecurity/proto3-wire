args:

let
  nixpkgs = builtins.fetchTarball {
    # nixos-21.11 as on 2023-06-16
    url = "https://github.com/NixOS/nixpkgs/archive/eabc38219184cc3e04a974fe31857d8e0eac098d.tar.gz";
    sha256 = "04ffwp2gzq0hhz7siskw6qh9ys8ragp7285vi1zh8xjksxn1msc5";
  };
in import nixpkgs ({ config = { }; } // args)
