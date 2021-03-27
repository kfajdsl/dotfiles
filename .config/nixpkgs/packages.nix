{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnome3.adwaita-icon-theme
    alacritty
    brightnessctl
    cachix
    cmake
    dbeaver
    discord
    docker
    fd
    feh
    ffmpeg
    firefox
    flameshot
    fzf
    gcc
    gimp
    gnumake
    hello
    htop
    iosevka-ss02-bin
    neofetch
    neovim
    gnome3.networkmanagerapplet
    nodejs
    pavucontrol
    picom
    polybar
    postgresql
    python3
    deluge
    redshift
    ripgrep
    rofi
    spotify
    sxhkd
    teams
    unzip
    usbutils
    virt-manager
    vivaldi
    vlc
    wmname
    xorg.xev
    xorg.xprop
    xsane
    xsel
    youtube-dl
  ];
}
