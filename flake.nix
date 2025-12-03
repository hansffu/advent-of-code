{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";
    devenv.url = "github:cachix/devenv";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{ self
    , nixpkgs
    , flake-parts
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.devenv.flakeModule
      ];

      perSystem =
        { self'
        , pkgs
        , config
        , ...
        }:
        {
          devenv.shells.default = {

            name = "AoC";
            languages.haskell = {
              enable = true;
              package = pkgs.haskell.compiler.ghc912;
              cabal.enable = true;
            };
            treefmt = {
              enable = true;
              config.programs = {
                nixpkgs-fmt.enable = true;
                cabal-fmt.enable = true;
                hlint.enable = true;

                ormolu = {
                  enable = true;
                  package = pkgs.haskellPackages.fourmolu;
                };
              };
            };
            packages = [ pkgs.haskellPackages.fourmolu ];
          };
          packages.default = self'.packages.advent-of-code;
        };
    };
}
