{ prsJSON, nixpkgs, proto3-wire }:
  let pkgs = import nixpkgs {};
      prs = builtins.fromJSON (builtins.readFile prsJSON);
      jobsets = pkgs.lib.mapAttrs (num: info:
        { enabled = 1;
          hidden = false;
          identifier = "PR-${num}";
          description = "PR ${num}: ${info.title}";
          nixexprinput = "src";
          nixexprpath = "release.nix";
          checkinterval = 300;
          schedulingshares = 1;
          enableemail = true;
          emailoverride = "hydra@awakenetworks.com";
          keepnr = 3;
          inputs =
            { src = { type = "git"; value = "https://github.com/${info.head.repo.owner.login}/${info.head.repo.name}.git ${info.head.ref}"; emailresponsible = false; };
              nixpkgs = { type = "git"; value = "https://github.com/NixOS/nixpkgs.git 7ae9da426924537755ce9164fd5b5f81ce16a1c3"; emailresponsible = false; };
            };
        }
      ) prs;
  in { jobsets = pkgs.writeText "jobsets.json" (builtins.toJSON jobsets); }
