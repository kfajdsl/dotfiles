{ pkgs, config, ... }:

{
  imports = [
    ./cachix.nix
    ./common.nix
    ./hardware-configuration.nix
  ];

  networking.hostName = "openair";
  networking.interfaces.eno1.useDHCP = true;
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];

  boot.extraModulePackages = with config.boot.kernelPackages; [
    rtl8821au
  ];
  boot.kernelModules = [ "rtl8821au" ];

  # No mouse acceleration
  services.xserver.config = ''
    Section "InputClass"
      Identifier "mouse accel"
      Driver "libinput"
      MatchIsPointer "on"
      Option "AccelProfile" "flat"
      Option "AccelSpeed" "0"
    EndSection
  '';

  services.logmein-hamachi.enable = true;

  services.xserver.displayManager.sessionCommands = "xrandr --output DVI-D-1 --left-of HDMI-1";
}
