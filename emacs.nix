{pkgs, ...}: {
	home-manager.users.zahara = { pkgs, ... }: {
		home.packages = with pkgs; [
	  		jetbrains-mono
			nil
			sbt
			metals
			tinymist  # Replaced typst-lsp with tinymist
			typst
		];
		programs.emacs = {
		  enable = true;
		  package = pkgs.emacs;
		  extraPackages = epkgs: [
			epkgs.chyla-theme
			epkgs.intellij-theme
			epkgs.lsp-mode
    		    	epkgs.lsp-ui
    		    	epkgs.company
		    	epkgs.scala-mode
        		epkgs.lsp-metals
			epkgs.sbt-mode
			epkgs.treemacs
			epkgs.treemacs-projectile
			epkgs.projectile
			epkgs.markdown-mode
			epkgs.org-modern
			epkgs.auctex
			epkgs.json-mode
			epkgs.yaml-mode
			epkgs.toml-mode
			epkgs.typst-mode
			epkgs.pdf-tools
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
			(add-hook 'scala-mode-hook #'lsp)

			;; Configure Metals for Scala
        		(require 'lsp-metals)
        		(setq lsp-metals-server-command "${pkgs.metals}/bin/metals-emacs")
        		(setq lsp-metals-scala-version "${scalaVersion}")

			;; Configure sbt-mode
			(require 'sbt-mode)
			(add-hook 'sbt-mode-hook #'company-mode)
	
			;; Configure lsp-ui
			(require 'lsp-ui)
			(setq lsp-ui-sideline-enable t
			      lsp-ui-doc-enable t)

			;; Enable company
			(require 'company)
			(add-hook 'nix-mode-hook #'company-mode)
        		(add-hook 'scala-mode-hook #'company-mode)

			(setq gc-cons-threshold 100000000)
			(setq lsp-idle-delay 0.500)
			(setq company-minimum-prefix-length 1
      			      company-idle-delay 0.0)

			(projectile-mode +1)

			;; Treemacs configuration
			(require 'treemacs)
			(global-set-key (kbd "C-x t t") 'treemacs)
			(setq treemacs-is-never-other-window t)
			(setq treemacs-width 35)
			(setq treemacs-follow-mode t)
			(setq treemacs-filewatch-mode t)
			(setq treemacs-project-follow-mode t)

			(add-hook 'projectile-mode-hook
				  (lambda ()
				    (when (and (boundp 'projectile-mode) projectile-mode)
				      (treemacs))))
			(require 'treemacs-projectile)

			;; Markdown configuration
			(require 'markdown-mode)
			(add-hook 'markdown-mode-hook #'company-mode)

			;; Org-mode configuration
			(require 'org-modern)
			(add-hook 'org-mode-hook #'org-modern-mode)
			(add-hook 'org-mode-hook #'company-mode)

			;; LaTeX configuration
			(require 'auctex)
			(setq TeX-auto-save t)
			(setq TeX-parse-self t)
			(add-hook 'LaTeX-mode-hook #'company-mode)

			;; JSON configuration
			(require 'json-mode)
			(add-hook 'json-mode-hook #'company-mode)
			(add-hook 'json-mode-hook #'lsp)

			;; YAML configuration
			(require 'yaml-mode)
			(add-hook 'yaml-mode-hook #'company-mode)
			(add-hook 'yaml-mode-hook #'lsp)

			;; TOML configuration
			(require 'toml-mode)
			(add-hook 'toml-mode-hook #'company-mode)

			;; Typst configuration
			(require 'typst-mode)
			(add-hook 'typst-mode-hook #'company-mode)
			(add-hook 'typst-mode-hook #'lsp)
			(setq lsp-typst-server "${pkgs.tinymist}/bin/tinymist")  # Updated to use tinymist

			;; PDF Tools configuration
			(require 'pdf-tools)
			(pdf-tools-install)

			;; Typst preview with pdf-tools
			(defun typst-start-watch ()
			  "Start typst watch for the current Typst file and open in pdf-tools."
			  (interactive)
			  (let ((file (buffer-file-name))
				(output (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
			    (when (and file (string-match "\\.typ$" file))
			      (start-process "typst-watch" nil
					     "${pkgs.typst}/bin/typst"
					     "watch" file output)
			      (find-file output))))
			(add-hook 'typst-mode-hook
				  (lambda ()
				    (local-set-key (kbd "C-c C-p") #'typst-start-watch)))

		  '';
		};
	};
}

