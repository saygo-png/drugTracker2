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
      switch = false;
      function = hl.buildStrictly;
    }
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
        enableSharedExecutables = false;
        doBenchmark = false;
        enableSharedLibraries = false;
        enableDeadCodeElimination = true;
        passthru.nixpkgs = pkgs;
        buildTools = (old.buildTools or []) ++ [pkgs.makeWrapper];

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
