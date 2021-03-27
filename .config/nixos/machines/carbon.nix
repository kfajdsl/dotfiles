{ pkgs, ... }:

{
  networking.hostName = "carbon";

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
