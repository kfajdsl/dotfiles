{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    pinned-nixpkgs.url = github:NixOS/nixpkgs/73ad5f9e147c0d2a2061f1d4bd91e05078dc0b58;
    emacs-overlay.url = github:nix-community/emacs-overlay/2c58c215a5ddfa2be00e349ee38545aa0547fcf2;
  };
  
  outputs = { self, nixpkgs, pinned-nixpkgs, emacs-overlay, ... }@attrs: {
    nixosConfigurations.openair = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = attrs;
      modules = [ ./openair.nix ];
    };
  };
}
