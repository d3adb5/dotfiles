require("functions")

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

augroup("common", {})

autocmd({ "VimLeave" }, {
  desc = "Return cursor to its original shape. Workaround for st.",
  callback = function () vim.o.guicursor = "a:hor20" end,
  group = "common"
})

autocmd({ "BufEnter", "BufWritePost" }, {
  desc = "Obtain the Git branch the file belongs to, if any.",
  callback = set_git_branch_var,
  group = "common"
})

autocmd({ "BufWritePost" }, {
  desc = "Run the 'make' program if it's set to something other than 'make'.",
  command = "if &makeprg != 'make' | make | endif",
  group = "common"
})

autocmd({ "FileType" }, {
  desc = "Autoformat paragraphs when editing Markdown or commit messages.",
  command = "set formatoptions+=a",
  pattern = "markdown,gitcommit",
  group = "common"
})

autocmd({ "FileType" }, {
  desc = "Set expandtab and tab width when editing Haskell code.",
  command = "set et ts=2 sw=2",
  pattern = "haskell",
  group = "common"
})
