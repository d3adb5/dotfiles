# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.kernelPackages = pkgs.linuxPackages_zen;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "optiplex";
  networking.networkmanager.enable = true;

  time.timeZone = "America/Vancouver";

  i18n.defaultLocale = "en_CA.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  fonts.fontconfig.defaultFonts = {
    monospace = [ "DejaVu Sans Mono" "IPAGothic" ];
    sansSerif = [ "Liberation Sans" "IPAGothic" ];
    serif = [ "DejaVu Serif" "IPAMincho" ];
  };

  hardware.keyboard.qmk.enable = true;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  nix.package = pkgs.nixFlakes;
  nix.extraOptions = "experimental-features = nix-command flakes";

  nixpkgs.config.allowUnfree = true;

  services.devmon.enable = true;

  services.xserver = {
    enable = true;

    logFile = "/tmp/x11.log";

    displayManager.gdm.enable = true;

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;

    monitorSection = ''
      # 2560x1080 @ 100.000 Hz Reduced Blank (CVT) field rate 100.000 Hz; hsync: 113.300 kHz; pclk: 299.11 MHz
      Modeline "100Hz"  299.11  2560 2568 2600 2640  1080 1119 1127 1133 +hsync -vsync
      Option "PreferredMode" "100Hz"
      Option "Monitor-DP-1" "Monitor[0]"
    '';

    layout = "us";
    xkbVariant = "intl";

    dpi = 76;
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.users.d3adb5 = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    file
    tree
    plex-media-player
    pinentry
    pinentry-curses
  ];

  environment.pathsToLink = [
    "/share/zsh"
  ];

  programs.zsh.enable = true;
  programs.mosh.enable = true;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
