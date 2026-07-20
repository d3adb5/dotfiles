require("functions")

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

augroup("common", {})

autocmd({ "VimLeave" }, {
  desc = "Return cursor to its original shape. Workaround for st.",
  callback = function () vim.o.guicursor = "a:hor20" end,
  group = "common"
})

autocmd({ "BufWritePost" }, {
  desc = "Run the 'make' program if it's set to something other than 'make'.",
  command = "if &makeprg != 'make' | silent make | endif",
  group = "common"
})

autocmd({ "FileType" }, {
  desc = "Autoformat paragraphs when editing Markdown or commit messages.",
  command = "setlocal formatoptions+=a",
  pattern = "markdown,gitcommit",
  group = "common"
})

autocmd({ "FileType" }, {
  desc = "Keep gq and insert-mode wrapping out of Markdown code blocks.",
  command = "setlocal formatexpr=v:lua.markdown_formatexpr()",
  pattern = "markdown",
  group = "common"
})

autocmd({ "CursorMoved", "CursorMovedI", "InsertEnter" }, {
  desc = "Suspend paragraph autoformatting while inside Markdown code blocks.",
  callback = function ()
    if vim.bo.filetype ~= "markdown" then return end

    local line = vim.api.nvim_win_get_cursor(0)[1]
    if markdown_lines_in_code_block(line, line) then
      if vim.bo.formatoptions:find("a") then
        vim.opt_local.formatoptions:remove("a")
        vim.b.autoformat_suspended = true
      end
    elseif vim.b.autoformat_suspended then
      vim.opt_local.formatoptions:append("a")
      vim.b.autoformat_suspended = nil
    end
  end,
  group = "common"
})

autocmd({ "FileType" }, {
  desc = "Set expandtab and tab width when editing Haskell code.",
  command = "set et ts=2 sw=2",
  pattern = "haskell",
  group = "common"
})
