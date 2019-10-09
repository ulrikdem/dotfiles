typeset -U path
path=(~/.local/bin $path)

export BROWSER=luakit
export EDITOR=/usr/bin/nvim
export FZF_DEFAULT_COMMAND='fd -L'
export FZF_DEFAULT_OPTS='--color 16 --bind tab:toggle-out,shift-tab:toggle-in,ctrl-space:toggle-all,change:top'
export WINEDLLOVERRIDES=winemenubuilder.exe=d
