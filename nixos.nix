{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    programs.drugtracker2.enable = lib.mkEnableOption "drugtracker2";
  };

  config = let
    drug = pkgs.callPackage ./package.nix {};
  in
    lib.mkIf config.drugtracker2.enable
    {
      environment.systemPackages = [drug];

      systemd.user.services.drug-reminder = {
        description = "Drug Reminder Service";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = let
            drugBin = "${drug}/bin/drug";
            bash = "${pkgs.bash}/bin/bash";
            notify-send = "${pkgs.libnotify}/bin/notify-send";
          in "${bash} -c '${drugBin} remind || ${notify-send} \"Drug Reminder\" \"Time to take your drugs!\"'";
          Environment = "DISPLAY=:0";
        };
      };

      systemd.user.timers.drug-reminder = {
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
