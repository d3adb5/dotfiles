{
  description = "d3adb5's NixOS configuration";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-23.05;
    home-manager.url = github:nix-community/home-manager/release-23.05;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = github:nix-community/nur;
  };

  outputs = { nixpkgs, home-manager, nur, ... }: rec {
    nixosConfigurations.optiplex = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        nur.nixosModules.nur
        home-manager.nixosModules.home-manager
        { nixpkgs.overlays = [nur.overlay]; }

        ./nix/configuration.nix
        ({config, pkgs, ...}: {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.d3adb5 = import ./nix/home.nix;
        })
      ];
    };

    homeConfigurations = {
      d3adb5 = nixosConfigurations.optiplex.config.home-manager.users.d3adb5.home;
    };
  };
}
