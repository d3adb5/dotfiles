export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=500000
export SAVEHIST=1000000

export EDITOR=nvim
export BROWSER=firefox
export NO_AT_BRIDGE=1

export GPG_TTY=$(tty)

export CC=/usr/bin/gcc
export CXX=/usr/bin/g++

export MAKEFLAGS=-j5
export CFLAGS=" -O3 -march=native "
export CXXFLAGS=" -O3 -march=native "

export LESSHISTFILE=-
export LESS="-R -S -# 4"

export GOPATH=~/.go

export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd -Dswing.aatext=true'

export MOSH_ESCAPE_KEY='~'

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'

path+=(
	"$HOME/.local/bin"
	"$HOME/.cabal/bin"
	"$HOME/.go/bin"
	"$HOME/.gem/ruby/2.6.0/bin"
	"$HOME/.gem/ruby/2.7.0/bin"
)

export PATH
