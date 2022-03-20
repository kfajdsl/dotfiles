{ config, pkgs, pinned-nixpkgs, emacs-overlay, ... }:

let
  pinnedPkgs = import (pinned-nixpkgs) {
    system = "x86_64-linux";
    overlays = [ emacs-overlay.overlay ];
  };
in
{
  nixpkgs.config = { allowUnfree = true; };

  nix = {
    package = pkgs.nixFlakes; # or versioned attributes like nix_2_7
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  programs.steam.enable = true;
  programs.java.enable = true;

  #virtualisation.virtualbox.host = {
  #  package = unstable-pin-2021-03-26.virtualbox;
  #  enable = true;
  #  enableExtensionPack = true;
  #};

  boot.kernelPackages = pkgs.linuxPackages_latest;  
  boot.kernelParams = [ "intel_pstate=active" ];

  boot.loader = {
    systemd-boot.enable = true;
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };
    grub.enable = false;
  };

  networking = {
    networkmanager.enable = true;
    # Use networkmanager's internal dhcp
    dhcpcd.enable = false;
  };

  # Use per interface DHCP, not global
  networking.useDHCP = false;

  services.syncthing = {
    enable = true;
    user = "sahan";
    dataDir = "/home/sahan/Sync/";
    configDir = "/home/sahan/.config/syncthing";
  };

  services.resolved = {
    enable = true;
  };

  services.lorri.enable = true;

  services.flatpak.enable = true;
  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-wlr
      ];
    };
  };

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    roboto-mono
    fira-code
  ];

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    # Enable the cups service, don't use sockets
    startWhenNeeded = false;
    drivers = [ pkgs.hplip ];
  };

  services.timesyncd.enable = true;


  # Enable sound.
  sound.enable = true;

  hardware = {
    enableAllFirmware = true;
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
    sane = {
      enable = true;
      extraBackends = [ pkgs.hplip ];
    };
    cpu.intel.updateMicrocode = true;
  };

  services = {
    xserver = {
      enable = true;
      displayManager.startx.enable = true;
      windowManager.bspwm.enable = true;
      libinput = {
        enable = true;
        touchpad.tapping = false;
      };
    };
  };

  programs.zsh.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sahan = {
    isNormalUser = true;
    home = "/home/sahan";
    extraGroups = [ "wheel" "vboxusers" "libvirtd" "docker" "scanner" "lp" ]; 
    shell = pkgs.zsh;
  };

  virtualisation.docker.enable = true;


  environment.systemPackages = with pkgs; [
    vim
    yadm
    git
    tmux
    w3m
    tailscale
    ((pinnedPkgs.emacsPackagesFor pinnedPkgs.emacsPgtkGcc).emacsWithPackages (epkgs: [ epkgs.vterm ]))
  ];

  programs.vim.defaultEditor = true;

  services.tailscale.enable = true;
  services.openssh.enable = true;


  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall = {
    allowedUDPPorts = [ 41641 ];
    enable = false;
  };


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

