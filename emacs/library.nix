{ config, lib, pkgs, ... }: let

in {
	disableToolBar = {
		configText = _: _: ''(tool-bar-mode -1)'';
	};
	disableMenuBar = {
		configText = _: _: ''(menu-bar-mode -1)'';
	};

	disableScrollBar = {
		configText = _: _: ''(scroll-bar-mode -1)'';
	};

	setDefaultFont = { family, height, fontPackage ? null }: {
		nixPackages = if fontPackage != null then [ fontPackage ] else [];
		configText = _: _: ''
			"(set-face-attribute 'default nil :family \"${family}\" :height ${toString (height * 100)})";	
		'';
	};

	enableLsp = { language, languageServerPackage, emacsPackageNames }: {
    		nixPackages = [ languageServerPackage ];
    		emacsPackages = epkgs: map (name: epkgs.${name}) emacsPackageNames;
    		configText = ''
      			(require 'lsp-mode)
      			(add-hook '${language}-mode-hook #'lsp)
    		'';
  	};

	enableMetals = scalaVersion: {
    		nixPackages = [ pkgs.metals ];
    		emacsPackages = epkgs: [
			epkgs.lsp-metals
		];
    		configText = ''
      			(require 'lsp-metals)
			(setq lsp-metals-server-command "${pkgs.metals}/bin/metals-emacs")
			(setq lsp-metals-scala-version "${scalaVersion}")
		'';
	};

	enableSbt = {
    		nixPackages = [ pkgs.sbt ];
    		emacsPackages = epkgs: [
			epkgs.sbt-mode
		];
    		configText = ''
      				
		'';
	};
}
