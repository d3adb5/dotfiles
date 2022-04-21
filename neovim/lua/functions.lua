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
