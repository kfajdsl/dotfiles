{ config, pkgs, ... }:

{
  imports =
    [ 
      ./common.nix
      ./machines/openair.nix
      ./hardware-configuration.nix
    ];
}

