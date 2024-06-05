typeset -U path
path=(~/.local/bin $path)

[[ -z $BROWSER ]] && type firefox >/dev/null && export BROWSER=firefox

[[ -x /usr/bin/nvim ]] && export EDITOR=/usr/bin/nvim MANPAGER="nvim +Man! +'set laststatus=1'" ABDUCO_CMD='nvim +terminal'

type less >/dev/null && export PAGER=less

type fd >/dev/null && export FZF_DEFAULT_COMMAND='fd -L --strip-cwd-prefix'
export FZF_DEFAULT_OPTS='--color 16,info:8 --marker * --tiebreak end,length '\
'--bind ctrl-w:backward-kill-word,alt-bs:unix-word-rubout,tab:toggle-out,shift-tab:toggle-in,ctrl-space:toggle-all,change:top'

[[ -f ~/.config/ripgreprc ]] && export RIPGREP_CONFIG_PATH=~/.config/ripgreprc

export HIGHLIGHT_OPTIONS="-t 4 -O xterm256"
[[ -f ~/.config/highlight/themes/vim.theme ]] && HIGHLIGHT_OPTIONS+=" -D $HOME/.config/highlight -s vim"

[[ -f ~/.config/dconf/profile ]] && export DCONF_PROFILE=~/.config/dconf/profile

export WINEDLLOVERRIDES=winemenubuilder.exe=
export DXVK_HUD=fps

if [[ -f ~/.zshenv.local ]]; then
    source ~/.zshenv.local
fi
