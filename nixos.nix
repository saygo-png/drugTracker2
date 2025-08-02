{pkgs, ...}: {
  systemd.user.services.drug-reminder = {
    description = "Drug Reminder Service";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = let
        bash = "${pkgs.bash}/bin/bash";
        notify-send = "${pkgs.libnotify}/bin/notify-send";
      in "${bash} -c 'drug remind || ${notify-send} \"Drug Reminder\" \"Time to take your drugs!\"'";
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
}
