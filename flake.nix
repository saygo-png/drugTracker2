{
  inputs = {
    treefmt-nix.url = "github:numtide/treefmt-nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
    eachSystemPkgs = f: nixpkgs.lib.genAttrs (import systems) (system: f pkgsFor.${system});
    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f system);
    treefmtEval = eachSystemPkgs (pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
  in {
    homeManagerModules.default = self.homeManagerModules.drugtracker2;
    homeManagerModules.drugtracker2 = import ./home-manager.nix;

    formatter = eachSystemPkgs (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
    packages = eachSystem (system: let
      niceHaskell = inputs.niceHaskell.outputs.niceHaskell.${system};
    in rec {
      drug = niceHaskell.mkPackage {
        flags = niceHaskell.mkFlags {doCheck = true;};
        packageRoot = ./.;
        cabalName = "drug2";
      };
      default = drug;
    });
  };
}
