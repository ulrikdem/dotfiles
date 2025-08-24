typeset -U path
path=(~/.local/bin $path)

[[ -z $BROWSER ]] && type firefox >/dev/null && export BROWSER=firefox

type nvim >/dev/null && export EDITOR=nvim MANPAGER="nvim +Man! +'set laststatus=1'" ABDUCO_CMD='nvim +terminal +startinsert'

type less >/dev/null && export PAGER=less
export LESSUTFCHARDEF=E0A0:p # Show the branch symbol in the prompt when opening kitty's scrollback in the pager

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

: ${__GLX_VENDOR_LIBRARY_NAME:=mesa} ${__EGL_VENDOR_LIBRARY_FILENAMES:=/usr/share/glvnd/egl_vendor.d/50_mesa.json} ${VK_LOADER_DRIVERS_DISABLE:=nvidia_icd.json}
export __GLX_VENDOR_LIBRARY_NAME __EGL_VENDOR_LIBRARY_FILENAMES VK_LOADER_DRIVERS_DISABLE

export DXVK_HUD=fps
export WINEDLLOVERRIDES=winemenubuilder.exe=

if [[ -f ~/.zshenv.local ]]; then
    source ~/.zshenv.local
fi
