{pkgs, ...}: {
	home-manager.users.zahara = { pkgs, ... }: {
		home.packages = with pkgs; [
	  		jetbrains-mono
			nil
			sbt
			metals
		];
		programs.emacs = {
		  enable = true;
		  package = pkgs.emacs;
		  extraPackages = epkgs: [
			epkgs.chyla-theme
			epkgs.intellij-theme
			epkgs.lsp-mode
    		    	epkgs.lsp-ui
    		    	epkgs.company  # Optional: for autocompletion
		    	epkgs.scala-mode # Add scala-mode for syntax highlighting
        		epkgs.lsp-metals    # Add Metals for Scala LSP support
			epkgs.sbt-mode  # For sbt build file support
			epkgs.treemacs
			epkgs.treemacs-projectile # Optional: for projectile integration
			epkgs.projectile # Add projectile
		  ];
		  extraConfig = let 
			scalaVersion = "3.7.0";
		  in ''
			(load-theme 'chyla t)
			(set-face-attribute 'default nil :font "JetBrains Mono-12")
			(tool-bar-mode -1)
			(menu-bar-mode -1)
			(scroll-bar-mode -1)

		    	;; Enable lsp-mode
			(require 'lsp-mode)
			(add-hook 'nix-mode-hook #'lsp)
			(add-hook 'scala-mode-hook #'lsp) ; Enable LSP for Scala files

			;; Configure Metals for Scala
        		(require 'lsp-metals)
        		(setq lsp-metals-server-command "${pkgs.metals}/bin/metals-emacs")
        		(setq lsp-metals-scala-version "${scalaVersion}") ; Specify Scala 3 version (adjust as needed)

			 ;; Configure sbt-mode
			(require 'sbt-mode)
			(add-hook 'sbt-mode-hook #'company-mode) ; Enable autocompletion in sbt files
	
			;; Optional: Configure lsp-ui for better visuals
			(require 'lsp-ui)
			(setq lsp-ui-sideline-enable t
			      lsp-ui-doc-enable t)

			;; Optional: Enable company for autocompletion
			(require 'company)
			(add-hook 'nix-mode-hook #'company-mode)
        		(add-hook 'scala-mode-hook #'company-mode) ; Enable company for Scala

			(setq gc-cons-threshold 100000000) ;; For lsp
			(setq lsp-idle-delay 0.500)
			(setq company-minimum-prefix-length 1
      			      company-idle-delay 0.0) ;; default is 0.2

			(projectile-mode +1) ;; Enable projectile globally

			;; Treemacs configuration
			(require 'treemacs)
			(global-set-key (kbd "C-x t t") 'treemacs) ; Bind treemacs toggle to C-x t t
			(setq treemacs-is-never-other-window t) ; Prevent treemacs from being selected
			(setq treemacs-width 35) ; Set treemacs sidebar width
			(setq treemacs-follow-mode t) ; Follow the current file in treemacs
			(setq treemacs-filewatch-mode t) ; Auto-refresh treemacs on file changes
			(setq treemacs-project-follow-mode t) ; Auto-switch to project root

			;; Automatically open treemacs when entering a project
			(add-hook 'projectile-mode-hook
				  (lambda ()
				    (when (and (boundp 'projectile-mode) projectile-mode)
				      (treemacs))))
			;; Optional: Integrate with projectile if you use it
			(require 'treemacs-projectile)

		  '';
		};
	};
}
