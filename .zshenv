typeset -U path
path=(~/.local/bin $path)

if [[ -x /usr/bin/luakit ]]; then
    export BROWSER=luakit
elif type firefox >/dev/null; then
    export BROWSER=firefox
fi

[[ -x /usr/bin/nvim ]] && export EDITOR=/usr/bin/nvim MANPAGER='nvim +Man!' ABDUCO_CMD='nvim +terminal'

type less >/dev/null && export PAGER=less

type fd >/dev/null && export FZF_DEFAULT_COMMAND='fd -L --strip-cwd-prefix'
export FZF_DEFAULT_OPTS='--color 16,info:8 --marker * --tiebreak end,length '\
'--bind ctrl-w:backward-kill-word,alt-bs:unix-word-rubout,tab:toggle-out,shift-tab:toggle-in,ctrl-space:toggle-all,change:top'

[[ -f ~/.config/ripgreprc ]] && export RIPGREP_CONFIG_PATH=~/.config/ripgreprc

export HIGHLIGHT_OPTIONS="-t 4 -O xterm256"
[[ -f ~/.config/highlight/themes/vim.theme ]] && HIGHLIGHT_OPTIONS+=" -D $HOME/.config/highlight -s vim"

[[ -f /usr/lib/qt/plugins/styles/libkvantum.so ]] && export QT_STYLE_OVERRIDE=kvantum

export WINEDLLOVERRIDES=winemenubuilder.exe=d

if [[ -f ~/.zshenv.local ]]; then
    source ~/.zshenv.local
fi
