{config, pkgs, ...}:
let
  custom-st = pkgs.st.overrideAttrs (oldAttrs: rec {
    src = pkgs.fetchFromGitHub {
      owner = "d3adb5";
      repo = "st";
      rev = "master";
      sha256 = "CE1fDW8yavz6BtCR8TI69aT80FTf9I8Atcz1wdR6qOE=";
    };
  });

  extra-ff-addons = import ./firefox-addons.nix { inherit pkgs; };
in {
  home.username = "d3adb5";
  home.homeDirectory = "/home/d3adb5";

  home.packages = with pkgs; [ home-manager stack bat qmk custom-st xclip ];

  home.stateVersion = "23.05";

  programs.home-manager.enable = true;
  programs.gpg.enable = true;
  programs.mpv.enable = true;

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    defaultKeymap = "vicmd";
  };

  programs.ssh.enable = true;
  programs.ssh.serverAliveInterval = 30;
  programs.ssh.matchBlocks = {
    github = {
      hostname = "github.com";
      user = "git";
    };
    gitlab = {
      hostname = "gitlab.com";
      user = "git";
    };
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

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

  programs.neovim.enable = true;

  xdg.configFile.nvim = {
    source = ../neovim;
    recursive = true;
  };

  programs.firefox = {
    enable = true;
    profiles.d3adb5 = {
      id = 0;
      isDefault = true;

      userChrome = builtins.readFile ../firefox/userChrome.css;
      settings."toolkit.legacyUserProfileCustomizations.stylesheets" = true;

      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        vimium ublock-origin sponsorblock
        extra-ff-addons.hide-scrollbars
        extra-ff-addons.tab-center-reborn
      ];
    };
  };

  xsession.enable = true;

  services.picom = {
    enable = true;
    shadow = false;
    vSync = true;
    fade = true;
    fadeDelta = 8;
    fadeSteps = [0.1 0.1];

    settings = {
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
    };
  };

  home.pointerCursor = {
    package = pkgs.xorg.xcursorthemes;
    name = "Neutral";
    size = 16;
    x11.enable = true;
    gtk.enable = true;
  };
}
