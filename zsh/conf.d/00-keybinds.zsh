bindkey -e

bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

bindkey '\e[1~'  beginning-of-line
bindkey '\e[4~'  end-of-line
bindkey '\e[5~'  beginning-of-history
bindkey '\e[3~'  delete-char
bindkey '\e[2~'  quoted-insert
bindkey '\eu'    backward-kill-line
bindkey '\ew'    backward-kill-word
bindkey '^[^[[D' backward-word
bindkey '^[^[[C' forward-word
