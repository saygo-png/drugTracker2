{
  system,
  pkgs,
  inputs,
  ...
}: let
  niceHaskell = inputs.niceHaskell.outputs.niceHaskell.${system};
in {
  drug = niceHaskell.mkPackage {
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
}
