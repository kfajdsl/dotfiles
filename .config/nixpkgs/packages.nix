{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ## Fonts 
    siji
    iosevka-ss02-bin
    unifont

    ## Dev Tools
    docker
    cmake
    gcc
    gnumake
    nodejs
    postgresql
    python3

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
    htop

    ## Tools
    brightnessctl
    fd
    feh
    ffmpeg
    flameshot
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
    youtube-dl

    ## Desktop
    picom
    polybar
    rofi
    sxhkd

    ## Misc
    gnome3.adwaita-icon-theme
  ];
}
