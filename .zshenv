typeset -U path
path=(~/.local/bin ~/.local/share/nvim/plugged/fzf/bin $path)

[[ -x /usr/bin/luakit ]] && export BROWSER=luakit
[[ -x /usr/bin/nvim ]] && export EDITOR=/usr/bin/nvim ABDUCO_CMD='nvim +terminal'

type -p fd >/dev/null && export FZF_DEFAULT_COMMAND='fd -L'
export FZF_DEFAULT_OPTS='--color 16,info:8 --tiebreak end,length --bind tab:toggle-out,shift-tab:toggle-in,ctrl-space:toggle-all,change:top,ctrl-z:ignore'

[[ -r ~/.config/ripgreprc ]] && export RIPGREP_CONFIG_PATH=~/.config/ripgreprc

export HIGHLIGHT_OPTIONS="-D $HOME/.config/highlight -t 4 -O xterm256"
[[ -r ~/.config/highlight/themes/vim.theme ]] && HIGHLIGHT_OPTIONS="$HIGHLIGHT_OPTIONS -s vim"

export QT_STYLE_OVERRIDE=kvantum

export WINEDLLOVERRIDES=winemenubuilder.exe=d
