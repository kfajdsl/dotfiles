{ config, pkgs, ... }:

let
  strToAttr = root: str: builtins.foldl' (a: b: a."${b}") root (pkgs.lib.strings.splitString "." str);
in
{
  # Read packages from a json file
  home.packages = with pkgs; [
    dbeaver
    alacritty
    firefox
    gimp
    neovim
    deluge
    teams
    spotify
    virt-manager
    vivaldi
    vlc
    xsane
    docker
    cmake
    gcc
    gnumake
    nodejs
    postgresql
    python3
    awscli2
    nodePackages.node2nix
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.prettier
    nix-index
    autoconf
    automake
    brightnessctl
    fd
    feh
    ffmpeg
    flameshot
    htop
    fzf
    neofetch
    pavucontrol
    networkmanagerapplet
    redshift
    ripgrep
    unzip
    usbutils
    wmname
    xorg.xev
    xorg.xprop
    xsel
    xorg.xcursorgen
    pkg-config
    youtube-dl
    lsof
    siji
    iosevka-ss02-bin
    unifont
    picom
    polybar
    rofi
    sxhkd
    gnome3.adwaita-icon-theme
    libreoffice
    vscode
    appimage-run
    protobuf
    libtool
    libvterm
    neovim-qt
    evince
    pulseeffects-legacy
    direnv
  ];

  services.udiskie = {
    enable = true;
    notify = true;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sahan";
  home.homeDirectory = "/home/sahan";

  fonts.fontconfig.enable = true;

  programs.info.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
