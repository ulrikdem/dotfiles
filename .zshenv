typeset -U path
path=(~/.local/bin $path)

[[ -z $BROWSER ]] && type firefox >/dev/null && export BROWSER=firefox

[[ -x /usr/bin/nvim ]] && export EDITOR=/usr/bin/nvim MANPAGER="nvim +Man! +'set laststatus=1'" ABDUCO_CMD='nvim +terminal'

type less >/dev/null && export PAGER=less

type fd >/dev/null && export FZF_DEFAULT_COMMAND='fd -L --strip-cwd-prefix'
export FZF_DEFAULT_OPTS='
    --color 16,border:white,info:gray
    --info inline-right
    --no-separator
    --no-multi-line
    --bind ctrl-z:ignore
    --bind ctrl-w:backward-kill-word,alt-bs:unix-word-rubout
    --bind change:top
    --bind tab:toggle-out,shift-tab:toggle-in
    --bind ctrl-a:select-all+accept
    --bind ctrl-s:jump,jump:deselect-all+accept
    --jump-labels neiohtsrad'

[[ -f ~/.config/dconf/profile ]] && export DCONF_PROFILE=~/.config/dconf/profile

export WINEDLLOVERRIDES=winemenubuilder.exe=
export DXVK_HUD=fps

if [[ -f ~/.zshenv.local ]]; then
    source ~/.zshenv.local
fi
