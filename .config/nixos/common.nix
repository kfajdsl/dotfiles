{ config, pkgs, ... }:

let
  unstable-pin-2021-03-26 = builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/d3f7e969b9860fb80750147aeb56dab1c730e756.tar.gz;
in
{
  imports =
    [
      "${unstable-pin-2021-03-26}/nixos/modules/virtualisation/virtualbox-host.nix"
    ];

  disabledModules = [ "virtualisation/virtualbox-host.nix" ];
  
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  virtualisation.virtualbox.host = {
    enable = true;
    enableExtensionPack = true;
  };

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
        tapping = false;
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
  ];
  programs.vim.defaultEditor = true;

  nixpkgs.config.allowUnfree = true;


  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}

