{ config, lib, pkgs, ... }: {
	home-manager.users.zahara = { pkgs, ... }: {
		programs.neovim = {
		      enable = true;
		      plugins = with pkgs.vimPlugins; [
			nvim-lspconfig
			nvim-cmp
			cmp-nvim-lsp
			cmp-buffer
			cmp-path
			cmp-cmdline
			haskell-tools-nvim
			luasnip
		      ];
		      extraConfig = ''
			lua << EOF
			  -- Setup LSPs
			  local lspconfig = require('lspconfig')

			  -- Idris2 LSP
			  lspconfig.idris2.setup {
			    cmd = { "${pkgs.idris2}/bin/idris2", "--lsp" }
			  }

			  -- Haskell LSP
			  require('haskell-tools').setup {
			    hls = {
			      cmd = { "${pkgs.haskell-language-server}/bin/haskell-language-server-wrapper", "--lsp" },
			      filetypes = { "haskell", "lhaskell" }
			    }
			  }

			  -- Nix LSP
			  lspconfig.nil_ls.setup {
			    cmd = { "${pkgs.nil}/bin/nil" },
			    settings = {
			      nix = {
				flake = {
				  autoArchive = true
				}
			      }
			    }
			  }

			  -- Optional: Setup autocompletion with nvim-cmp
			  local cmp = require('cmp')
			  cmp.setup {
			    snippet = {
			      expand = function(args)
				require('luasnip').lsp_expand(args.body) -- If using luasnip
			      end,
			    },
			    mapping = cmp.mapping.preset.insert({
			      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
			      ['<C-f>'] = cmp.mapping.scroll_docs(4),
			      ['<C-Space>'] = cmp.mapping.complete(),
			      ['<C-e>'] = cmp.mapping.abort(),
			      ['<CR>'] = cmp.mapping.confirm({ select = true }),
			    }),
			    sources = cmp.config.sources({
			      { name = 'nvim_lsp' },
			      { name = 'buffer' },
			      { name = 'path' },
			    })
			  }
		      '';
	      };
	};
}
