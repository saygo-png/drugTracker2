{
  picker ? null,
  jsonConfig ? null,
  # --
  pkgs,
}: let
  pickerPkg =
    if picker != null
    then picker
    else pkgs.fzf;

  hl = pkgs.haskell.lib;

  listSwitchFunc = [
    {
      switch = true;
      function = pkgs.haskellPackages.generateOptparseApplicativeCompletions ["drug"];
    }
  ];

  onSwitchApplyFunc = set: object:
    if set.switch
    then set.function object
    else object;

  pkg = pkgs.haskellPackages.developPackage {
    root = ./.;
    returnShellEnv = false;

    modifier = drv:
      hl.overrideCabal drv (old: {
        doHaddock = false;
        enableExecutableProfiling = false;
        enableLibraryProfiling = false;
        doBenchmark = false;
        enableDeadCodeElimination = true;
        passthru.nixpkgs = pkgs;
        buildTools = (old.buildTools or []) ++ [pkgs.makeWrapper];

        configureFlags =
          (old.configureFlags or [])
          ++ [
            "--ghc-options=-O2"
            "--enable-optimization=2"
            "--enable-split-sections"
            "--enable-executable-stripping"
          ];

        prePatch =
          (old.prePatch or "")
          + (
            if jsonConfig != null
            then "cp ${jsonConfig} config.json"
            else ""
          );

        postInstall =
          (old.postInstall or "")
          + ''
            wrapProgram $out/bin/drug \
              --prefix PATH : ${pkgs.lib.makeBinPath [pickerPkg pkgs.libnotify]}
          '';
      });
  };
in
  pkgs.lib.foldr onSwitchApplyFunc pkg listSwitchFunc
