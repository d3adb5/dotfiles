# mosh
#
# Relays its arguments to mosh, but if it finds its first argument in one of set
# values, it will execute 'tmuxinator start master' upon connection. It's here
# as a shortcut to access the 'master' session on tmux every time I connect to
# one of my servers.
#
# TODO: Move the server list to an environment variable.

mosh() {
  local servers=( akita odroid tokyo inaba )

  if ((servers[(Ie)$1])); then
    command mosh "$@" -- tmuxinator start master
  else
    command mosh "$@"
  fi
}
