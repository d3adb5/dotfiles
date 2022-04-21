require("settings")
require("highlights")
require("functions")
require("mappings")
require("autocommands")

require("packer").startup(function ()
  use "wbthomason/packer.nvim"

  -- {{{ essential plugins
  use "tpope/vim-repeat"
  use "tpope/vim-eunuch"
  use "tpope/vim-unimpaired"
  use "tpope/vim-surround"
  use "tpope/vim-commentary"
  use "Sirver/ultisnips"

  use { "junegunn/fzf.vim",
    config = function ()
      vim.keymap.set("n", "<C-f>", "<cmd>Files<cr>")
      vim.keymap.set("n", "<C-l>", "<cmd>Lines<cr>")
    end
  }

  use { "junegunn/vim-easy-align",
    event = "VimEnter",
    config = function ()
      vim.keymap.set("x", "ga", "<plug>(EasyAlign)")
      vim.keymap.set("n", "ga", "<plug>(EasyAlign)")
    end
  }

  use { "honza/vim-snippets",
    config = function()
      vim.g.UltiSnipsExpandTrigger       = "<Tab>"
      vim.g.UltiSnipsJumpForwardTrigger  = "<C-j>"
      vim.g.UltiSnipsJumpBackwardTrigger = "<C-k>"
      vim.g.UltiSnipsEditSplit           = "vertical"
      vim.g.UltiSnipsSnippetDirectories  = { "UltiSnips", "snips" }
    end
  }
  -- }}}
  -- {{{ additional highlighting and convenience
  use { "lilydjwg/colorizer",      ft = {"markdown", "html", "scss", "css"} }
  use { "plasticboy/vim-markdown", ft = "markdown" }
  use { "dkarter/bullets.vim",     ft = "markdown" }
  use { "junegunn/goyo.vim",       ft = "markdown" }
  use { "keith/tmux.vim",          event = "VimEnter .tmux.conf" }

  use { "martinda/Jenkinsfile-vim-syntax", event = "VimEnter Jenkinsfile" }

  use { "machakann/vim-highlightedyank",
    config = function ()
      vim.g.highlightedyank_highlighted_duration = 1000
    end
  }

  use { "ntpeters/vim-better-whitespace",
    config = function ()
      vim.g.strip_whitespace_on_save = 1
    end
  }
  -- }}}
  -- {{{ git related plugins
  use { "tpope/vim-rhubarb", cond = in_a_git_repo }
  use { "junegunn/gv.vim",   cond = in_a_git_repo }

  use { "tpope/vim-fugitive",
    cond = in_a_git_repo,
    config = function ()
      vim.keymap.set("n", "<leader>gd", ":Gvdiffsplit!<cr>")
      vim.keymap.set("n", "gch",        ":diffget //2")
      vim.keymap.set("n", "gcl",        ":diffget //3")
    end
  }

  use { "mhinz/vim-signify",
    cond = in_a_git_repo,
    config = function ()
      vim.api.nvim_set_hl(0, "SignifySignAdd",    { ctermfg = "green"  })
      vim.api.nvim_set_hl(0, "SignifySignDelete", { ctermfg = "red"    })
      vim.api.nvim_set_hl(0, "SignifySignChange", { ctermfg = "yellow" })
    end
  }
  -- }}}
  -- {{{ the nerd tree
  use { "scrooloose/nerdtree",
    config = function ()
      vim.g.NERDTreeDirArrowExpandable = "+"
      vim.g.NERDTreeDirArrowCollapsible = "-"
      vim.g.NERDTreeMinimalUI = 1
      vim.g.NERDTreeIgnore = { "\\.hi$", "\\.o$", "\\.class$" }
    end
  }

  use { "Xuyuanp/nerdtree-git-plugin", after = "nerdtree" }
  -- }}}

  -- {{{ completion, linting, language servers
  use "mfussenegger/nvim-lint"

  use { "hrsh7th/nvim-cmp",
    after = "nvim-lspconfig",
    requires = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "quangnguyen30192/cmp-nvim-ultisnips"
    },
    config = function () require("completion") end
  }

  use { "neovim/nvim-lspconfig",
    config = function ()
      vim.keymap.set("n", "K",     vim.lsp.buf.hover)
      vim.keymap.set("n", "gd",    vim.lsp.buf.definition)
      vim.keymap.set("n", "gi",    vim.lsp.buf.implementation)
      vim.keymap.set("n", "gr",    vim.lsp.buf.references)
      vim.keymap.set("n", "gD",    vim.lsp.buf.declaration)
      vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help)

      vim.keymap.set("n", "<leader>f",  vim.lsp.buf.formatting)
      vim.keymap.set("n", "<leader>D",  vim.lsp.buf.type_definition)
      vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename)
      vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action)

      vim.keymap.set("n", "<C-k>", vim.lsp.diagnostic.goto_prev)
      vim.keymap.set("n", "<C-j>", vim.lsp.diagnostic.goto_next)
    end
  }
  -- }}}
end)
