{ pkgs, ... }:

let
  # UI Configuration: Disable UI elements and set theme
  uiConfig = { theme, themePackage }: {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.${themePackage} ];
    configText = ''
      (load-theme '${theme} t)
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
    '';
  };

  # Font Configuration: Set font family and size
  fontConfig = { family, size, fontPackage }: {
    nixPackages = [ fontPackage ];
    emacsPackages = epkgs: [];
    configText = "(set-face-attribute 'default nil :font \"${family}-${toString size}\")";
  };

  # LSP Base Configuration: General LSP and company setup
  lspBaseConfig = {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.lsp-mode epkgs.lsp-ui epkgs.company ];
    configText = ''
      (require 'lsp-mode)
      (require 'lsp-ui)
      (setq lsp-ui-sideline-enable t
            lsp-ui-doc-enable t)
      (require 'company)
      (setq gc-cons-threshold 100000000)
      (setq lsp-idle-delay 0.500)
      (setq company-minimum-prefix-length 1
            company-idle-delay 0.0)
    '';
  };

  # Language LSP Configuration: Enable LSP and company for a language
  languageLspConfig = { mode, nixPackages } : {
    nixPackages = nixPackages;
    emacsPackages = epkgs: [];
    configText = ''
      (add-hook '${mode}-mode-hook #'lsp)
      (add-hook '${mode}-mode-hook #'company-mode)
    '';
  };

  # Scala Development: Metals and sbt-mode
  scalaConfig = { scalaVersion }: {
    nixPackages = [ pkgs.sbt pkgs.metals ];
    emacsPackages = epkgs: [ epkgs.scala-mode epkgs.lsp-metals epkgs.sbt-mode ];
    configText = ''
      (require 'lsp-metals)
      (setq lsp-metals-server-command "${pkgs.metals}/bin/metals-emacs")
      (setq lsp-metals-scala-version "${scalaVersion}")
      (require 'sbt-mode)
      (add-hook 'sbt-mode-hook #'company-mode)
      (add-hook 'scala-mode-hook #'lsp)
      (add-hook 'scala-mode-hook #'company-mode)
    '';
  };

  # Project Management: Projectile and Treemacs
  projectConfig = {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.projectile epkgs.treemacs epkgs.treemacs-projectile ];
    configText = ''
      (projectile-mode +1)
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
    '';
  };

  # Markdown Support
  markdownConfig = {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.markdown-mode ];
    configText = "(add-hook 'markdown-mode-hook #'company-mode)";
  };

  # Org Mode Support
  orgConfig = {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.org-modern ];
    configText = ''
      (require 'org-modern)
      (add-hook 'org-mode-hook #'org-modern-mode)
      (add-hook 'org-mode-hook #'company-mode)
    '';
  };

  # LaTeX Support
  latexConfig = {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.auctex ];
    configText = ''
      (require 'auctex)
      (setq TeX-auto-save t)
      (setq TeX-parse-self t)
      (add-hook 'LaTeX-mode-hook #'company-mode)
    '';
  };

  # JSON Support with LSP
  jsonConfig = {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.json-mode ];
    configText = ''
      (add-hook 'json-mode-hook #'company-mode)
      (add-hook 'json-mode-hook #'lsp)
    '';
  };

  # YAML Support with LSP
  yamlConfig = {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.yaml-mode ];
    configText = ''
      (add-hook 'yaml-mode-hook #'company-mode)
      (add-hook 'yaml-mode-hook #'lsp)
    '';
  };

  # TOML Support
  tomlConfig = {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.toml-mode ];
    configText = "(add-hook 'toml-mode-hook #'company-mode)";
  };

  # Typst Support with LSP and Preview
  typstConfig = {
    nixPackages = [ pkgs.tinymist pkgs.typst ];
      emacsPackages = epkgs: [ epkgs.typst-ts-mode ];
      configText = ''
        (require 'typst-ts-mode)
        (add-hook 'typst-ts-mode-hook #'lsp)
        (add-hook 'typst-ts-mode-hook #'company-mode)
        (setq lsp-typst-server "${pkgs.tinymist}/bin/tinymist")
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
        (add-hook 'typst-ts-mode-hook
                  (lambda ()
                    (local-set-key (kbd "C-c C-p") #'typst-start-watch)))
      '';
  };

  # PDF Tools
  pdfToolsConfig = {
    nixPackages = [];
    emacsPackages = epkgs: [ epkgs.pdf-tools ];
    configText = ''
      (require 'pdf-tools)
      (pdf-tools-install)
    '';
  };

  # Magit with Forge
    magitConfig = {
      nixPackages = [ pkgs.git ];
      emacsPackages = epkgs: [ epkgs.magit epkgs.forge ];
      configText = ''
        (require 'magit)
        (require 'forge)
        (global-set-key (kbd "C-x g") 'magit-status)
        (setq forge-add-default-bindings t)
      '';
   };
in
{
  home-manager.users.zahara = { pkgs, ... }: {
    imports = [ ./emacs-module.nix ];
    emacsConfigUnits = [
      (uiConfig { theme = "chyla"; themePackage = "chyla-theme"; })
      (fontConfig { family = "JetBrains Mono"; size = 12; fontPackage = pkgs.jetbrains-mono; })
      lspBaseConfig
      (languageLspConfig { mode = "nix"; nixPackages = [ pkgs.nil ]; })
      (scalaConfig { scalaVersion = "3.7.0"; })
      projectConfig
      markdownConfig
      orgConfig
      latexConfig
      jsonConfig
      yamlConfig
      tomlConfig
      typstConfig
      pdfToolsConfig
    ];
  };
}

