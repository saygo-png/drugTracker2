{
  inputs = {
    treefmt-nix.url = "github:numtide/treefmt-nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    niceHaskell = {
      url = "github:saygo-png/nice-nixpkgs-haskell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
  };

  outputs = {
    nixpkgs,
    systems,
    self,
    ...
  } @ inputs: let
    pkgsFor = nixpkgs.lib.genAttrs (import systems) (system:
      import nixpkgs {
        inherit system;
      });

    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f system pkgsFor.${system});
    treefmtEval = eachSystem (_: pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
  in {
    homeManagerModules.default = self.homeManagerModules.drugtracker2;
    homeManagerModules.drugtracker2 = import ./home-manager.nix;

    packages = eachSystem (system: pkgs: let
      niceHaskell = inputs.niceHaskell.outputs.niceHaskell.${system};
    in rec {
      drug = niceHaskell.mkPackage rec {
        flags = niceHaskell.mkFlags {doCheck = false;};
        packageRoot = ./.;
        cabalName = "drug2";
        compiler = "ghc912";
        developPackageArgs = {
          overrides = _: super: {
            say = pkgs.haskell.lib.dontCheck super.say;
          };
        };
      };
      default = drug;
    });

    formatter = eachSystem (_: pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
  };
}
