{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
      ./zerotier.nix
      ./vpn.nix
      ./hyprland.nix
      ./waybar.nix
      ./zen.nix
      ./tablet_drivers.nix
      ./neovim.nix
    ];

  programs.ssh.startAgent = true;

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/nvme0n1";
  boot.loader.grub.useOSProber = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Moscow";

  fonts.fontDir.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "ru_RU.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "ru_RU.UTF-8";
    LC_IDENTIFICATION = "ru_RU.UTF-8";
    LC_MEASUREMENT = "ru_RU.UTF-8";
    LC_MONETARY = "ru_RU.UTF-8";
    LC_NAME = "ru_RU.UTF-8";
    LC_NUMERIC = "ru_RU.UTF-8";
    LC_PAPER = "ru_RU.UTF-8";
    LC_TELEPHONE = "ru_RU.UTF-8";
    LC_TIME = "ru_RU.UTF-8";
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "ru";
    variant = "";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.zahara = {
    isNormalUser = true;
    description = "Zahara";
    extraGroups = [ "networkmanager" "wheel" "input" ];
    packages = with pkgs; [];
  };

  home-manager.backupFileExtension = "backup";
  home-manager.users.zahara = { pkgs, ... }: {
  	imports = [  
		./hyprland-config.nix
      		./emacs/newEmacs.nix
	];
  	home.packages = [ pkgs.atool pkgs.httpie ];
  	programs.bash.enable = true;

  	# The state version is required and should stay at the version you
  	# originally installed.
  	home.stateVersion = "24.11";
  	programs.git = {
    		enable = true;
    		userName  = "Katze";
    		userEmail = "googletan@mail.ru";
  	};
  };

  # Enable automatic login for the user.
  services.getty.autologinUser = "zahara";

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  fonts.packages = with pkgs; [
	noto-fonts
  ] ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  environment.systemPackages = with pkgs; [
     vlc
     unzip
     unrar
     jetbrains.pycharm-community
     jetbrains.idea-community-bin

     vesktop
     (discord.override {
       withVencord = true;
     })
     jetbrains.idea-ultimate
     keepass

     heroic
     modrinth-app

     ghostty
     firefox
     nautilus

     git

     rofi-wayland # app launcher.

     telegram-desktop
     
     grim
     slurp
     wl-clipboard
     

     loupe # image viewer

     swww # wallpapper

     # minecraft modding launcher
     prismlauncher

     # apple music
     cider

     #java
     zulu17
     zulu21
     
     gnome-system-monitor

     whatsie

     yarn
     sbt

     ffmpeg

     torrential

     docker
     kubernetes

     krita
   ];

  services.xserver.videoDrivers = lib.mkDefault ["modesetting"];
  hardware.amdgpu.initrd.enable = lib.mkDefault true;
  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  swapDevices = [
  	{ device = "/swapfile"; }
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
