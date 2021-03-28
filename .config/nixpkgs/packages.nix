{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ## Programs
    dbeaver
    alacritty
    discord
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

    ## Dev Tools
    docker
    cmake
    gcc
    gnumake
    nodejs
    postgresql
    python3
    awscli2

    ## Tools
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
    hello
    neofetch
    pavucontrol
    gnome3.networkmanagerapplet
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

    ## Fonts 
    siji
    iosevka-ss02-bin
    unifont

    ## Desktop
    picom
    polybar
    rofi
    sxhkd

    ## Themes
    gnome3.adwaita-icon-theme
  ];
}
