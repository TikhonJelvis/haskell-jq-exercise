{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        { self', pkgs, ... }:
        {

          # Typically, you just want a single project named "default". But
          # multiple projects are also possible, each using different GHC version.
          haskellProjects.default = {
            # The base package set representing a specific GHC version.
            # By default, this is pkgs.haskellPackages.
            # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
            # basePackages = pkgs.haskellPackages;

            # Extra package information. See https://community.flake.parts/haskell-flake/dependency
            #
            # Note that local packages are automatically included in `packages`
            # (defined by `defaults.packages` option).
            #
            packages = {
              # aeson.source = "1.5.0.0";      # Override aeson to a custom version from Hackage
              # shower.source = inputs.shower; # Override shower to a custom source path
            };
            settings = {
              #  aeson = {
              #    check = false;
              #  };
              #  relude = {
              #    haddock = false;
              #    broken = false;
              #  };
            };

            devShell = {
              # Programs you want to make available in the shell.
              # Default programs can be disabled by setting to 'null'
              #
              # (ie ghcid = null;)
              tools = hp: {
                inherit (hp)
                  cabal-fmt
                  fourmolu
                  ;

                inherit (pkgs)
                  nixfmt-rfc-style
                  ;
              };

              # Check that haskell-language-server works
              # hlsCheck.enable = true; # Requires sandbox to be disabled
            };
          };

          # haskell-flake doesn't set the default package, but you can do it here.
          packages.default = self'.packages.example;

          formatter = pkgs.nixfmt;
        };
    };
}
