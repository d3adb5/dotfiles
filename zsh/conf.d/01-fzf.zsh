util-fzf() {
  FZF_DEFAULT_OPTS="--height 40% $FZF_DEFAULT_OPTS --tiebreak=index" fzf "$@"
}

util-file-list() {
  while IFS= read -r entry; do
    local mark="$(awk '{print $1}' <<< "$entry")"
    local file="$(awk '{print $NF}' <<< "$entry")"
    if [[ "$mark" = '??' ]]; then
      find "$file" | xargs -n 1 printf '?? %s\n'
    else
      echo "$entry"
    fi
  done
}

util-in-git-repo() {
  [[ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" == "true" ]] \
    && return 0 || return 128
}

util-pick-branch() {
  local references="$(git for-each-ref --format='%(refname:short)' refs)"
  local logcmd='git log --color=always --graph --oneline {}'
  util-fzf "$@" --preview "$logcmd" <<< "$references"
}

util-pick-commit() {
  local showcmd='git show --color=always --raw $(cut -d" " -f1 <<< {})'
  local commits="$(git log --oneline $target_branch)"
  util-fzf "$@" --preview "$showcmd" <<< "$commits" | cut -d' ' -f1
}

util-pick-file() {
  local gitmark="\$(awk '{print \$1}' <<< {})"
  local filename="\$(awk '{print \$NF}' <<< {})"
  local diffcmd="git diff --color=always $filename"
  local showcmd="[ $gitmark = '??' ] && cat $filename || $diffcmd"
  git status --porcelain | util-file-list \
    | util-fzf "$@" --preview "$showcmd" | awk '{print $NF}'
}

fzf-git-pick-commit-from-branch() {
  util-in-git-repo || { zle redisplay && return 128 }
  setopt localoptions noglobsubst noposixbuiltins pipefail 2>/dev/null
  local branch=$(util-pick-branch +m); zle reset-prompt
  [[ -n "$branch" ]] || { zle redisplay && return 0 }
  local commits=($(target_branch="$branch" util-pick-commit -m))
  LBUFFER="${LBUFFER}${commits[@]}"
  zle reset-prompt
}

fzf-git-pick-commit() {
  util-in-git-repo || { zle redisplay && return 128 }
  setopt localoptions noglobsubst noposixbuiltins pipefail 2>/dev/null
  local commits=($(util-pick-commit -m))
  LBUFFER="${LBUFFER}${commits[@]}"
  zle reset-prompt
}

fzf-git-pick-branch() {
  util-in-git-repo || { zle redisplay && return 128 }
  setopt localoptions noglobsubst noposixbuiltins pipefail 2>/dev/null
  local branches=($(util-pick-branch -m))
  LBUFFER="${LBUFFER}${branches[@]}"
  zle reset-prompt
}

fzf-git-pick-file() {
  util-in-git-repo || { zle redisplay && return 128 }
  setopt localoptions noglobsubst noposixbuiltins pipefail 2>/dev/null
  local files=($(util-pick-file -m))
  LBUFFER="${LBUFFER}${files[@]}"
  zle reset-prompt
}

zle     -N   fzf-git-pick-commit-from-branch
bindkey '^N' fzf-git-pick-commit-from-branch
zle     -N   fzf-git-pick-commit
bindkey '^F' fzf-git-pick-commit
zle     -N   fzf-git-pick-branch
bindkey '^K' fzf-git-pick-branch
zle     -N   fzf-git-pick-file
bindkey '^V' fzf-git-pick-file

if [[ -d /usr/share/fzf ]]; then
  source /usr/share/fzf/completion.zsh
  source /usr/share/fzf/key-bindings.zsh
elif [[ -d /usr/share/doc/fzf/examples ]]; then
  source /usr/share/doc/fzf/examples/completion.zsh
  source /usr/share/doc/fzf/examples/key-bindings.zsh
fi
