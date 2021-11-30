# User-provided scripts to source. Should be under $XDG_CONFIG_HOME/zsh-usr.

if [[ -d "$XDG_CONFIG_HOME/zsh-usr" ]]; then
  for zsh_config in "$XDG_CONFIG_HOME"/zsh-usr/*; do
    source "$zsh_config"
  done
fi
