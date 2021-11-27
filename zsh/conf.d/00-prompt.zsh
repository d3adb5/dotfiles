autoload -Uz promptinit
autoload -Uz vcs_info

setopt prompt_subst

precmd() {
  vcs_info
}

+vi-git-untracked() {
  if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) == 'true' ]] && \
      git status --porcelain | grep -q -m 1 '??'; then
    hook_com[misc]=' %F{9}!%f'
  fi
}

zstyle ':vcs_info:git*+set-message:*' hooks git-untracked
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr ' %F{10}+%f'
zstyle ':vcs_info:git:*' unstagedstr ' %F{9}•%f'
zstyle ':vcs_info:git:*' formats '→ %F{14}%b%f%m%u%c'
zstyle ':vcs_info:git:*' actionformats '→ %F{14}%b %F{13}%a%f'

PROMPT='%F{11}%n %F{12}%M %F{10}%~%f ${vcs_info_msg_0_}
%(!.#.») '

PS2='  » '

promptinit
