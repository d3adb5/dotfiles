function _G.statusline_git_branch()
  if vim.fn.exists("*FugitiveHead") == 1 then
    return vim.fn.FugitiveHead()
  end
  return ""
end

function in_a_git_repo()
  return os.execute("git status >/dev/null 2>/dev/null") == 0
end

function _G.markdown_lines_in_code_block(first, last)
  local ok, parser = pcall(vim.treesitter.get_parser, 0, "markdown")
  if not ok or parser == nil then return false end

  local root = parser:parse()[1]:root()

  for lnum = first, last do
    local node = root:descendant_for_range(lnum - 1, 0, lnum - 1, 0)
    while node do
      local type = node:type()
      if type == "fenced_code_block" or type == "indented_code_block" then
        return true
      end
      node = node:parent()
    end
  end

  return false
end

function _G.markdown_formatexpr()
  local first = vim.v.lnum
  local last  = first + vim.v.count - 1
  return markdown_lines_in_code_block(first, last) and 0 or 1
end

function bootstrap_lazynvim()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable",
      lazypath
    })
  end
  vim.opt.rtp:prepend(lazypath)
end
