require("settings")
require("highlights")
require("functions")
require("mappings")
require("autocommands")

vim.loader.enable()
bootstrap_lazynvim()

require("lazy").setup({
  -- {{{ essential plugins
  "tpope/vim-repeat",
  "tpope/vim-eunuch",
  "tpope/vim-unimpaired",
  "tpope/vim-surround",
  "tpope/vim-commentary",
  "Sirver/ultisnips",

  { "junegunn/fzf.vim",
    dependencies = { "junegunn/fzf" },
    config = function ()
      vim.keymap.set("n", "<C-f>", "<cmd>Files<cr>")
      vim.keymap.set("n", "<C-l>", "<cmd>Lines<cr>")
    end
  },

  { "junegunn/vim-easy-align",
    event = "VimEnter",
    config = function ()
      vim.keymap.set("x", "ga", "<plug>(EasyAlign)")
      vim.keymap.set("n", "ga", "<plug>(EasyAlign)")
    end
  },

  { "honza/vim-snippets",
    config = function()
      vim.g.UltiSnipsExpandTrigger       = "<Tab>"
      vim.g.UltiSnipsJumpForwardTrigger  = "<C-j>"
      vim.g.UltiSnipsJumpBackwardTrigger = "<C-k>"
      vim.g.UltiSnipsEditSplit           = "vertical"
      vim.g.UltiSnipsSnippetDirectories  = { "UltiSnips", "snips" }
    end
  },
  -- }}}
  -- {{{ additional highlighting and convenience
  "cappyzawa/starlark.vim",
  "andrewstuart/vim-kubernetes",

  { "lilydjwg/colorizer",     ft = {"markdown", "html", "scss", "css"} },
  { "tpope/vim-markdown",     ft = "markdown" },
  { "dkarter/bullets.vim",    ft = "markdown" },
  { "junegunn/goyo.vim",      ft = "markdown" },
  { "hashivim/vim-terraform", ft = "terraform" },
  { "towolf/vim-helm",        ft = "yaml" },
  { "keith/tmux.vim",         event = "VimEnter .tmux.conf" },

  { "martinda/Jenkinsfile-vim-syntax", event = "VimEnter Jenkinsfile" },

  { "machakann/vim-highlightedyank",
    config = function ()
      vim.g.highlightedyank_highlighted_duration = 1000
    end
  },

  { "ntpeters/vim-better-whitespace",
    config = function ()
      vim.g.strip_whitespace_on_save = 1
    end
  },
  -- }}}
  -- {{{ git related plugins
  { "tpope/vim-rhubarb", cond = in_a_git_repo },
  { "junegunn/gv.vim",   cond = in_a_git_repo },

  { "tpope/vim-fugitive",
    cond = in_a_git_repo,
    config = function ()
      vim.keymap.set("n", "<leader>gd", ":Gvdiffsplit!<cr>")
      vim.keymap.set("n", "gch",        ":diffget //2")
      vim.keymap.set("n", "gcl",        ":diffget //3")
    end
  },

  { "mhinz/vim-signify",
    cond = in_a_git_repo,
    config = function ()
      vim.api.nvim_set_hl(0, "SignifySignAdd",    { ctermfg = "green"  })
      vim.api.nvim_set_hl(0, "SignifySignDelete", { ctermfg = "red"    })
      vim.api.nvim_set_hl(0, "SignifySignChange", { ctermfg = "yellow" })
    end
  },
  -- }}}
  -- {{{ the nerd tree
  { "scrooloose/nerdtree",
    dependencies = {
      "Xuyuanp/nerdtree-git-plugin"
    },
    config = function ()
      vim.g.NERDTreeDirArrowExpandable = "+"
      vim.g.NERDTreeDirArrowCollapsible = "-"
      vim.g.NERDTreeMinimalUI = 1
      vim.g.NERDTreeIgnore = { "\\.hi$", "\\.o$", "\\.class$" }
    end,

    enabled = false
  },
  -- }}}
  -- {{{ completion, linting, language servers
  { "mfussenegger/nvim-lint",
    config = function ()
      vim.api.nvim_create_autocmd({ "BufWritePost" }, {
        desc = "Attempt linting when files are written.",
        group = "common",
        callback = function ()
          require("lint").try_lint()
        end
      })
    end
  },

  { "CopilotC-Nvim/CopilotChat.nvim",
    dependencies = {
      { "github/copilot.vim",
        config = function ()
          vim.g.copilot_filetypes = { markdown = true, yaml = true, gitcommit = true }
        end
      },
      { "nvim-lua/plenary.nvim", branch = "master" },
    },
    build = "make tiktoken",
    opts = {},
  },

  { "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "quangnguyen30192/cmp-nvim-ultisnips",
      "neovim/nvim-lspconfig"
    },
    config = function ()
      require("completion")
    end
  },

  { "mfussenegger/nvim-jdtls",
    ft = "java",
    config = function ()
      require("jdtls").start_or_attach({
        cmd = {"/usr/bin/jdtls"},
        root_dir = vim.fs.dirname(
          vim.fs.find({"gradlew", "pom.xml", ".git"}, { upward = true})[1]
        )
      })
    end,

    enabled = false
  }
  -- }}}
})
