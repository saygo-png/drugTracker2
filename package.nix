{
  jsonConfig ? null,
  picker ? pkgs.fzf,
  pkgs,
  lib,
  niceHaskell,
  ...
}: let
  hasJsonConfig = jsonConfig != null;
in
  niceHaskell.mkPackage {
    flags = niceHaskell.mkFlags {doCheck = false;};
    packageRoot = ./.;
    cabalName = "drug2";
    compiler = "ghc912";
    developPackageArgs.overrides = _: super: {
      say = pkgs.haskell.lib.dontCheck super.say;
    };
    overrideCabalOverride = old: {
      prePatch =
        (old.prePatch or "")
        + lib.optionalString hasJsonConfig "cp ${jsonConfig} config.json";

      buildTools = (old.buildTools or []) ++ [pkgs.makeWrapper];

      postInstall =
        (old.postInstall or "")
        + ''
          wrapProgram $out/bin/drug \
            --prefix PATH : ${pkgs.lib.makeBinPath [picker pkgs.libnotify]}
        '';
    };
  }
