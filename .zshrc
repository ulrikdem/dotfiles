setopt chase_links
setopt no_bg_nice
setopt no_check_jobs
setopt rc_quotes

zstyle ':completion:*' menu select
zstyle ':completion:*:manuals.*' insert-sections false

type dircolors >/dev/null && eval $(dircolors ~/.config/dir_colors)
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
setopt list_packed no_list_types

zle_highlight=(suffix:fg=8)

if typeset -f isgrml >/dev/null; then
    zstyle :prompt:grml:left:setup items user at host fullpath vcs rc newline arrow
    zstyle :prompt:grml:right:setup use-rprompt false

    zstyle :prompt:grml:left:items:at pre %F{white}
    zstyle :prompt:grml:left:items:at post %f
    zstyle :prompt:grml:left:items:host pre %B%F{blue}
    zstyle :prompt:grml:left:items:host post %f%b
    grml_theme_add_token fullpath '%~ ' %B %b

    zstyle ':vcs_info:*' formats '%8F[%F{blue}%b%c%u%8F] %f'
    zstyle ':vcs_info:*' actionformats '%8F[%F{blue}%b%8F:%F{magenta}%a%c%u%8F] %f'
    zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat %b%8F:%F{blue}%r
    zstyle ':vcs_info:*' stagedstr %8F:%F{green}S
    zstyle ':vcs_info:*' unstagedstr %8F:%F{red}U
    zstyle ':vcs_info:*' check-for-changes true

    grml_theme_add_token arrow '%F{blue}» %f'
    PS2='%8F%_ %F{blue}» %f'

    bindkey '^P' history-beginning-search-backward-end
    bindkey '^N' history-beginning-search-forward-end

    bindkey -s '^S' '^X.'
    abk[LC]='--color=always |& less -r'
fi

[[ -d ~/.dotfiles.git ]] && alias dotfiles='GIT_DIR=~/.dotfiles.git GIT_WORK_TREE=~ zsh'

type abduco >/dev/null && alias abduco="abduco -e '^H'"
type diff >/dev/null && alias diff='diff --color=auto'
type gcc >/dev/null && alias gcc='gcc -std=c17 -Wall -Wextra -Wconversion'
type g++ >/dev/null && alias g++='g++ -std=c++20 -Wall -Wextra -Wconversion'
type nvim >/dev/null && alias vim=nvim
type nvr >/dev/null && [[ -n $NVIM_LISTEN_ADDRESS ]] && alias vim=nvr
type ssh >/dev/null && alias ssh='TERM=xterm-256color ssh'

stty -ixon

[[ -r /etc/profile.d/vte.sh ]] && source /etc/profile.d/vte.sh

if [[ -r ~/.local/share/nvim/plugged/fzf/shell/key-bindings.zsh ]]; then
    source ~/.local/share/nvim/plugged/fzf/shell/key-bindings.zsh

    if type fzf >/dev/null && type fd >/dev/null; then
        fzf-file-widget-helper() {
            local query=${1##*/}
            local dir=${1:0:$#1-$#query}
            while [[ -n $dir && ! -d $~dir ]]; do
                dir=${dir%/}
                query=${dir##*/}/$query
                dir=${1:0:$#1-$#query}
            done
            cd -- ${~dir:-.}
            unset REPORTTIME
            local item
            fd -L0 | fzf --read0 --height 40% --reverse --prompt ${dir:-./} -q "$query" -m --print0 |
                while read -rd $'\0' item; do
                    echo -n "$dir${(q)item} "
                done
        }

        fzf-file-widget() {
            local tokens=(${(z)LBUFFER})
            [[ $LBUFFER[-1] =~ '\s' ]] && tokens+=
            local results=$(fzf-file-widget-helper $tokens[-1])
            [[ -n $results ]] && LBUFFER=${LBUFFER:0:$#LBUFFER-$#tokens[-1]}$results
            zle reset-prompt
        }

        fzf-cd-widget() {
            unset REPORTTIME
            cd -- "$(fd -L0td | fzf --read0 --height 40% --reverse --prompt 'cd ')"
            REPORTTIME=5
            zle fzf-redraw-prompt
        }
    fi
fi
