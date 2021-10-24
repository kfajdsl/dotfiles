{ pkgs, ... }:

{
  networking.hostName = "carbon";
  networking.interfaces.wlp3s0.useDHCP = true;
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];

  hardware = {
    firmware = [ pkgs.broadcom-bt-firmware ];
    bluetooth = {
      enable = true;
    };
    pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
    trackpoint = {
      enable = true;
      emulateWheel = true;
      speed = 40;
      sensitivity = 250;
    };
  };

  services = {
    tlp.enable = true;
    blueman.enable = true;
  };
}
