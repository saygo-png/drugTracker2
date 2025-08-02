{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    treefmt-nix.url = "github:numtide/treefmt-nix";
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
    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f pkgsFor.${system});
    treefmtEval = eachSystem (pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
  in {
    nixosModules.default = self.nixosModules.drugtracker2;
    nixosModules.drugtracker2 = import ./nixos.nix;

    formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
    packages = eachSystem (pkgs: rec {
      drug = pkgs.callPackage ./package.nix {};
      default = drug;
    });
  };
}
