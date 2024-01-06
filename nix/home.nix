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
  nixpkgs.config.allowUnfree = true;

  home.username = "d3adb5";
  home.homeDirectory = "/home/d3adb5";

  home.packages = with pkgs; [
    stack bat qmk custom-st xclip spotify
    unzip unrar p7zip unp
    gimp
  ];

  home.stateVersion = "23.05";

  programs.home-manager.enable = true;
  programs.password-store.enable = true;
  programs.mpv.enable = true;
  programs.gpg.enable = true;
  programs.pqiv.enable = true;

  programs.tmux = {
    enable = true;
    escapeTime = 0;
    terminal = "tmux-256color";
    plugins = with pkgs.tmuxPlugins; [ pain-control sessionist ];
    extraConfig = builtins.readFile ../tmux/tmux.conf;
  };

  services.gpg-agent = {
    enable = true;
    enableZshIntegration = true;
    pinentryFlavor = "curses";
    defaultCacheTtl = 60;
    maxCacheTtl = 120;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = false;
    syntaxHighlighting.enable = true;
    initExtra = ''
      for configFile in "$XDG_CONFIG_HOME"/zsh/*; do
        source "$configFile"
      done
    '';
  };

  xdg.configFile.zsh = {
    source = ../zsh/conf.d;
    recursive = true;
  };

  xdg.enable = true;
  xdg.userDirs = {
    enable = true;
    documents = "${config.home.homeDirectory}/documents";
    download = "${config.home.homeDirectory}/downloads";
    desktop = "${config.home.homeDirectory}/desktop";
    music = "${config.home.homeDirectory}/music";
    pictures = "${config.home.homeDirectory}/pictures";
    templates = "${config.home.homeDirectory}/templates";
    videos = "${config.home.homeDirectory}/videos";
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
