# External tools / plugins / programs that are not considered must-haves.

autosuggestions="/usr/share/zsh/plugins/zsh-autosuggestions"
tmuxinator="/usr/lib/ruby/gems/3.0.0/gems/tmuxinator-3.0.1"

source "$autosuggestions"/zsh-autosuggestions.zsh 2>/dev/null
source "$tmuxinator"/completion/tmuxinator.zsh 2>/dev/null

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/bin/terraform terraform
