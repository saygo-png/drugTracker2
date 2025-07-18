{pkgs}: let
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
      hl.overrideCabal drv (_: {
        doHaddock = false;
        enableDeadCodeElimination = true;
        passthru.nixpkgs = pkgs;
      });
  };
in
  pkgs.lib.foldr onSwitchApplyFunc pkg listSwitchFunc
