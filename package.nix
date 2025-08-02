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
      hl.overrideCabal drv (old: {
        doHaddock = false;
        enableDeadCodeElimination = true;
        passthru.nixpkgs = pkgs;
        # Add build tools for wrapping
        buildTools = (old.buildTools or []) ++ [ pkgs.makeWrapper ];

        # Add post-install phase to wrap the binary
        postInstall = (old.postInstall or "") + ''
          wrapProgram $out/bin/drug \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.fzf pkgs.libnotify ]}
        '';
      });
  };
in
  pkgs.lib.foldr onSwitchApplyFunc pkg listSwitchFunc
