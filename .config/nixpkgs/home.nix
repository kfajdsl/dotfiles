{ config, pkgs, ... }:

let
  strToAttr = root: str: builtins.foldl' (a: b: a."${b}") root (pkgs.lib.strings.splitString "." str);
in
{
  # Read packages from a json file
  home.packages = map (x: strToAttr pkgs x) (builtins.fromJSON (builtins.readFile ./packages.json));

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sahan";
  home.homeDirectory = "/home/sahan";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";

  fonts.fontconfig.enable = true;

  programs.info.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    extraPackages = (epkgs: [ epkgs.vterm ] );
  };

}
