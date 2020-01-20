typeset -U path
path=(~/.local/bin $path)

[[ -x /usr/bin/luakit ]] && export BROWSER=luakit
[[ -x /usr/bin/nvim ]] && export EDITOR=/usr/bin/nvim ABDUCO_CMD=nvim

type -p fd >/dev/null && export FZF_DEFAULT_COMMAND='fd -L'
export FZF_DEFAULT_OPTS='--color 16 --bind tab:toggle-out,shift-tab:toggle-in,ctrl-space:toggle-all,change:top,ctrl-z:ignore'

export HIGHLIGHT_OPTIONS="-D $HOME/.config/highlight -t 4 -O xterm256"
[[ -r ~/.config/highlight/themes/vim.theme ]] && HIGHLIGHT_OPTIONS="$HIGHLIGHT_OPTIONS -s vim"

export QT_STYLE_OVERRIDE=kvantum

export WINEDLLOVERRIDES=winemenubuilder.exe=d
