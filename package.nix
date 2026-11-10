{
  jsonConfig ? null,
  picker ? pkgs.fzf,
  pkgs,
  lib,
  niceHaskell,
  ...
}: let
  hasJsonConfig = jsonConfig != null;
  binaryName = "drug";
in
  niceHaskell.mkPackageNoIFD {
    src = ./.;
    cabal2nix = ./cabal.nix;
    shellCompletions = [binaryName];
    overrideCabal = old: {
      prePatch =
        (old.prePatch or "")
        + lib.optionalString hasJsonConfig "cp ${jsonConfig} config.json";

      buildTools = (old.buildTools or []) ++ [pkgs.makeWrapper];

      postInstall =
        (old.postInstall or "")
        + ''
          wrapProgram $out/bin/${binaryName} \
            --prefix PATH : ${pkgs.lib.makeBinPath [picker pkgs.libnotify]}
        '';
    };
  }
