{ config, lib, pkgs, ... }:

let
  cfg = config.programs.neovim.lsp;
in
{
  options.programs.neovim.lsp = {
    idris2 = {
      enable = lib.mkEnableOption "Idris2 LSP support";
    };
    haskell = {
      enable = lib.mkEnableOption "Haskell LSP support";
    };
    nix = {
      enable = lib.mkEnableOption "Nix LSP support";
    };
  };

	config = {
    programs.neovim = {
			plugins = with pkgs.vimPlugins; [
				nvim-lspconfig
        {
          plugin = nvim-lspconfig; # The LSP configuration plugin
          config = ''
            lua << EOF
              -- Setup Idris2 LSP if enabled
              ${lib.optionalString cfg.idris2.enable ''
                require('lspconfig').idris2.setup{
                  cmd = { "${pkgs.idris2}/bin/idris2", "--lsp" }
                }
              ''}

              -- Setup Haskell LSP if enabled
              ${lib.optionalString cfg.haskell.enable ''
                require('lspconfig').hls.setup{
                  cmd = { "${pkgs.haskell-language-server}/bin/haskell-language-server-wrapper", "--lsp" }
                }
              ''}
            EOF
          '';
        }
      ];
    };
	};
}
