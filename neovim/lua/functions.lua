function _G.statusline_git_branch()
  if vim.fn.exists("*FugitiveHead") == 1 then
    return vim.fn.FugitiveHead()
  end
  return ""
end

function in_a_git_repo()
  return os.execute("git status >/dev/null 2>/dev/null") == 0
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
