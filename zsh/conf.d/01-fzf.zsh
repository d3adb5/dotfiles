fzf-git-pick-commit-from-branch() {
  setopt localoptions noglobsubst noposixbuiltins pipefail 2>/dev/null
  local branch commits ret
  branch=$(git for-each-ref --format='%(refname:short)' refs \
    | FZF_DEFAULT_OPTS="--height 40% $FZF_DEFAULT_OPTS --tiebreak=index" fzf \
        +m --preview 'git log --graph --color=always --oneline {}')
  ret=$?
  zle reset-prompt
  if [ -z "$branch" ]; then
    return $ret
  fi
  commits=($(git log --oneline "$branch" \
    | FZF_DEFAULT_OPTS="--height 40% $FZF_DEFAULT_OPTS --tiebreak=index" fzf \
        -m --preview 'git show --color=always --raw $(cut -d" " -f1 <<< {})' \
    | cut -d' ' -f1)
  )
  ret=$?
  LBUFFER="${LBUFFER}${commits[@]}"
  zle reset-prompt
  return $ret
}

fzf-git-pick-commit() {
  setopt localoptions noglobsubst noposixbuiltins pipefail 2>/dev/null
  local commits ret
  commits=($(git log --oneline \
    | FZF_DEFAULT_OPTS="--height 40% $FZF_DEFAULT_OPTS --tiebreak=index" fzf \
        -m --preview 'git show --color=always --raw $(cut -d" " -f1 <<< {})' \
    | cut -d' ' -f1)
  )
  ret=$?
  LBUFFER="${LBUFFER}${commits[@]}"
  zle reset-prompt
  return $ret
}

fzf-git-pick-branch() {
  setopt localoptions noglobsubst noposixbuiltins pipefail 2>/dev/null
  local branches ret
  branches=($(git for-each-ref --format='%(refname:short)' refs \
    | FZF_DEFAULT_OPTS="--height 40% $FZF_DEFAULT_OPTS --tiebreak=index" fzf \
        -m --preview 'git log --graph --color=always --oneline {}' )
  )
  ret=$?
  LBUFFER="${LBUFFER}${branches[@]}"
  zle reset-prompt
  return $ret
}

zle     -N   fzf-git-pick-commit-from-branch
bindkey '^N' fzf-git-pick-commit-from-branch
zle     -N   fzf-git-pick-commit
bindkey '^F' fzf-git-pick-commit
zle     -N   fzf-git-pick-branch
bindkey '^K' fzf-git-pick-branch

source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh
