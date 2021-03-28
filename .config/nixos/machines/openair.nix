{ pkgs, ... }:

{
  networking.hostName = "openair";
  networking.interfaces.enp0s25.useDHCP = true;

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

  services.xserver.displayManager.sessionCommands = "xrandr --output DVI-D-1 --left-of HDMI-1"
}
