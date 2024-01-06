{
  description = "d3adb5's NixOS configuration";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-23.11;
    home-manager.url = github:nix-community/home-manager/release-23.11;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = github:nix-community/nur;
  };

  outputs = { nixpkgs, home-manager, nur, ... }: rec {
    nixosConfigurations.optiplex = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [./nix/configuration.nix];
    };

    homeConfigurations.d3adb5 = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        { nixpkgs.overlays = [nur.overlay]; }
        ./nix/home.nix
      ];
    };
  };
}
