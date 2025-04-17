{ config, lib, pkgs, ... }:

let
  zenBrowserRepo = fetchGit {
    url = "https://github.com/0xc000022070/zen-browser-flake.git";
    # Optionally specify a specific revision:
    rev = "05e0ba13f847ab7521fc85664d85727a5c32fdb5"; 
  };
  
  flakeCompat = import (fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/12c64ca55c1014cdc1b16ed5a804aa8576601ff2.tar.gz";
    sha256 = "0jm6nzb83wa6ai17ly9fzpqc40wg1viib8klq8lby54agpl213w5";
  });
  
  zenBrowserFlake = flakeCompat { src = zenBrowserRepo; };
  twilight = zenBrowserFlake.defaultNix.packages.${pkgs.system}.twilight;
in
{
  environment.systemPackages = with pkgs; [
    twilight
  ];
}

