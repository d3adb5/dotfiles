function get_git_branch()
  local git_command = "git status --porcelain -b " .. vim.fn.expand("%")
    .. " 2>/dev/null | tr . ' ' | sed 1q | awk '{print $2}' | tr -d '\n'"

  local process = assert(io.popen(git_command, "r"))
  local output  = assert(process:read('*a'))

  process:close()

  return output
end

function set_git_branch_var()
  vim.b.gitbranch = get_git_branch()
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
