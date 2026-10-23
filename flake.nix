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
    niceHaskell,
    self,
    ...
  } @ inputs: let
    pkgsFor = nixpkgs.lib.genAttrs (import systems) (system: import nixpkgs {inherit system;});
    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f system pkgsFor.${system});

    treefmtEval = eachSystem (_: pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
  in {
    homeManagerModules.default = self.homeManagerModules.drugtracker2;
    homeManagerModules.drugtracker2 = (import ./home-manager.nix) niceHaskell;

    packages = eachSystem (system: pkgs: rec {
      drug = pkgs.callPackage ./package.nix {niceHaskell = niceHaskell.outputs.niceHaskell.${system};};
      default = drug;
    });

    devShells = eachSystem (system: pkgs: let
      hPkgs = pkgs.haskellPackages;
    in {
      default = pkgs.mkShell {
        packages = [
          self.formatter.${system}
          pkgs.zlib
          hPkgs.cabal-install
          hPkgs.ghc
          hPkgs.haskell-language-server
        ];
      };
    });

    formatter = eachSystem (system: _: treefmtEval.${system}.config.build.wrapper);
  };
}
