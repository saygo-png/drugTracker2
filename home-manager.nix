{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.programs.drugtracker2;
  drug = pkgs.callPackage ./package.nix {};
in {
  options = {
    programs.drugtracker2 = {
      enable = lib.mkEnableOption "drugtracker2";

      systemdIntegration = {
        enable =
          lib.mkEnableOption "drugtracker2"
          // {
            default = true;
          };

        remindFrequency = lib.mkOption {
          type = lib.types.str;
          default = "hourly";
          description = ''
            Systemd timer period to create for scheduled {command}`drug remind`.

            The format is described in {manpage}`systemd.time(7)`.
          '';
        };
      };

      package = lib.mkPackageOption {inherit drug;} "drug" {nullable = true;};
    };
  };

  config =
    lib.mkIf cfg.enable
    {
      home.packages = lib.mkIf (cfg.package != null) [cfg.package];

      systemd.user.services.drugtracker2 = lib.mkIf cfg.systemdIntegration.enable {
        Unit.Description = "Drugtracker2 Service";
        Service = {
          Type = "oneshot";
          ExecStart = let
            drugBin = "${drug}/bin/drug";
            bash = "${pkgs.bash}/bin/bash";
            notify-send = "${pkgs.libnotify}/bin/notify-send";
          in "${bash} -c '${drugBin} remind && ${notify-send} \"Drug Reminder\" \"Time to take your drugs!\" || exit 0'";
          # TODO: improve error handling to show errors.
          Environment = "DISPLAY=:0";
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
