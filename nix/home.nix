{ config, pkgs, ...}:

{
  home.username = "d3adb5";
  home.homeDirectory = "/home/d3adb5";

  home.stateVersion = "23.05";

  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "d3adb5";
    userEmail = "me@d3adb5.net";

    aliases = {
      please = "push --force-with-lease";
      fall = "fetch --all";
      frune = "fetch --prune --all";
    };
  };
}
