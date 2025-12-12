niceHaskell: {
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.programs.drugtracker2;
in {
  options = {
    programs.drugtracker2 = {
      enable = lib.mkEnableOption "drugtracker2";

      columnString = lib.mkOption {
        type = lib.types.singleLineStr;
        default = " | ";
        description = "Text used to draw a column";
      };

      rowString = lib.mkOption {
        type = lib.types.singleLineStr;
        default = "-";
        description = "Text used to draw a row";
      };

      picker = lib.mkOption {
        type = lib.types.package;
        default = pkgs.fzf;
        description = "Picker package used for \"drug take\"";
      };

      pickerBinName = lib.mkOption {
        type = lib.types.singleLineStr;
        default = lib.getExe cfg.picker;
        description = "Picker binary name used for \"drug take\"";
      };

      systemdIntegration = {
        enable =
          lib.mkEnableOption "drugtracker2"
          // {
            default = true;
          };

        remindFrequency = lib.mkOption {
          type = lib.types.singleLineStr;
          default = "hourly";
          description = ''
            Systemd timer period to create for scheduled {command}`drug remind`.

            The format is described in {manpage}`systemd.time(7)`.
          '';
        };
      };
    };
  };

  config = let
    jsonFormat = pkgs.formats.json {};
    jsonConfig = jsonFormat.generate "config.json" {
      inherit (cfg) columnString rowString;
      picker = cfg.pickerBinName;
    };
    drugConfigured = pkgs.callPackage ./package.nix {
      inherit (cfg) picker;
      inherit jsonConfig;
      niceHaskell = niceHaskell.outputs.niceHaskell.${pkgs.stdenv.hostPlatform.system};
    };
  in
    lib.mkIf cfg.enable
    {
      home.packages = [drugConfigured];

      systemd.user.services.drugtracker2 = lib.mkIf cfg.systemdIntegration.enable {
        Unit.Description = "Drugtracker2 Service";
        Service = {
          Type = "oneshot";
          ExecStart = let
            drugBin = "${drugConfigured}/bin/drug";
            bash = "${pkgs.bash}/bin/bash";
          in "${bash} -c '${drugBin} remind || exit 0'";
          # TODO: improve error handling to show errors.
        };
      };

      systemd.user.timers.drugtracker2 = lib.mkIf cfg.systemdIntegration.enable {
        Unit.Description = "Run drug remind every hour";
        Install.WantedBy = ["timers.target"];
        Timer = {
          OnCalendar = cfg.systemdIntegration.remindFrequency;
          Persistent = true;
          RandomizedDelaySec = 60;
        };
      };
    };
}
