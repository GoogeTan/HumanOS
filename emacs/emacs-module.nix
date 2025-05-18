{ config, lib, pkgs, ... }:

let
  types = lib.types;
  mkOption = lib.mkOption;

  configUnitType = types.submodule {
    options = {
      nixPackages = mkOption {
        type = types.listOf types.package;
        default = [];
        description = "Список Nix зависимостей";
      };
      emacsPackages = mkOption {
        type = types.uniq (types.functionTo (types.listOf types.package));
        default = epkgs: [];
        description = "Функция из emacsPackages.";
      };
      configText = mkOption {
        type = types.str;
        default = pkgs: epkgs: "";
        description = "Функция из pkgs, emacs в код на Elisp";
      };
    };
  };
in
{
  options.emacsConfigUnits = mkOption {
    type = types.listOf configUnitType;
    default = [];
    description = "Список модулей";
  };

  config = {
    programs.emacs = {
      enable = true;
      extraPackages = epkgs: lib.concatMap (unit: unit.emacsPackages epkgs) config.emacsConfigUnits;
      extraConfig = lib.concatStringsSep "\n" (map (unit: unit.configText) config.emacsConfigUnits);
    };
    home.packages = lib.concatMap (unit: unit.nixPackages) config.emacsConfigUnits;
  };
}
