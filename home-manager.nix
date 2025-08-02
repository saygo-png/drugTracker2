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

      systemdIntegration =
        lib.mkEnableOption "drugtracker2"
        // {
          default = true;
        };

      package = lib.mkPackageOption {inherit drug;} "drug" {nullable = true;};
    };
  };

  config =
    lib.mkIf cfg.enable
    {
      home.packages = lib.mkIf (cfg.package != null) [cfg.package];

      systemd.user.services.drug-reminder = lib.mkIf cfg.systemdIntegration {
        description = "Drug Reminder Service";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = let
            drugBin = "${drug}/bin/drug";
            bash = "${pkgs.bash}/bin/bash";
            notify-send = "${pkgs.libnotify}/bin/notify-send";
          in "${bash} -c '${drugBin} remind && ${notify-send} \"Drug Reminder\" \"Time to take your drugs!\"'";
          Environment = "DISPLAY=:0";
        };
      };

      systemd.user.timers.drug-reminder = lib.mkIf cfg.systemdIntegration {
        description = "Run Drug Reminder Every Hour";
        wantedBy = ["timers.target"];
        timerConfig = {
          OnCalendar = "hourly";
          Persistent = true;
          RandomizedDelaySec = 60;
        };
      };
    };
}
