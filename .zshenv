declare -U path
path=(~/.local/bin $path)

[[ -x /usr/bin/luakit ]] && export BROWSER=luakit
[[ -x /usr/bin/nvim ]] && export EDITOR=/usr/bin/nvim MANPAGER='nvim +Man!' ABDUCO_CMD='nvim +terminal'

type fd >/dev/null && export FZF_DEFAULT_COMMAND='fd -L'
export FZF_DEFAULT_OPTS='--color 16,info:8 --marker * --tiebreak end,length --bind change:top,tab:toggle-out,shift-tab:toggle-in,ctrl-space:toggle-all'

[[ -f ~/.config/ripgreprc ]] && export RIPGREP_CONFIG_PATH=~/.config/ripgreprc

export HIGHLIGHT_OPTIONS="-D $HOME/.config/highlight -t 4 -O xterm256"
[[ -f ~/.config/highlight/themes/vim.theme ]] && HIGHLIGHT_OPTIONS="$HIGHLIGHT_OPTIONS -s vim"

[[ -f /usr/lib/qt/plugins/styles/libkvantum.so ]] && export QT_STYLE_OVERRIDE=kvantum

export WINEDLLOVERRIDES=winemenubuilder.exe=d
