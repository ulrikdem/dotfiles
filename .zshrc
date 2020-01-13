setopt chase_links
setopt no_bg_nice
setopt no_check_jobs
setopt rc_quotes

zstyle ':completion:*' menu select
zstyle ':completion:*:manuals.*' insert-sections false

type dircolors >/dev/null && eval $(dircolors ~/.config/dir_colors)
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
setopt list_packed no_list_types

if typeset -f isgrml >/dev/null; then
    zstyle :prompt:grml:left:setup items user at host static-time path vcs rc newline arrow
    zstyle :prompt:grml:right:setup use-rprompt false

    grml_theme_add_token static-time -f static-time
    static-time() {
        REPLY="%F{white}${(%):-%*}%f "
    }

    zstyle ':vcs_info:*' formats '%F{white}[%F{green}%b%F{white}]%f '
    zstyle ':vcs_info:*' actionformats '%F{white}[%F{green}%b%F{white}:%F{red}%a%F{white}]%f '

    grml_theme_add_token arrow '%F{blue}» %f'
    PS2='%F{white}%_%F{blue} » %f'

    bindkey '^P' history-beginning-search-backward-end
    bindkey '^N' history-beginning-search-forward-end

    bindkey -s '^S' '^X.'
    abk[LC]='--color=always |& less -r'
fi

alias rmcdir-r='cd ..; rm -r -- $OLDPWD || cd -- $OLDPWD'
alias rmcdir-rf='cd ..; rm -rf -- $OLDPWD || cd -- $OLDPWD'

type diff >/dev/null && alias diff='diff --color=auto'
type gcc >/dev/null && alias gcc='gcc -Wall -Wextra'
type g++ >/dev/null && alias g++='g++ -Wall -Wextra'
type git >/dev/null && [[ -d ~/.dotfiles.git ]] &&
    alias dotfiles-git='git --git-dir $HOME/.dotfiles.git --work-tree $HOME -c status.showUntrackedFiles=no'
type nvim >/dev/null && alias vim=nvim
type ssh >/dev/null && alias ssh='TERM=xterm-256color ssh'
type udevil >/dev/null && alias pmount='udevil mount'
type udevil >/dev/null && alias pumount='udevil umount'

stty -ixon

[[ -r /etc/profile.d/vte.sh ]] && source /etc/profile.d/vte.sh

if [[ -r /usr/share/fzf/key-bindings.zsh ]]; then
    source /usr/share/fzf/key-bindings.zsh

    if type fd >/dev/null; then
        fzf-file-widget-helper() {
            local query=${1##*/}
            local dir=${1:0:${#1}-${#query}}
            while [[ -n $dir && ! -d $dir ]]; do
                dir=${dir%/}
                query=${dir##*/}/$query
                dir=${1:0:${#1}-${#query}}
            done
            cd -- "$dir"
            unset REPORTTIME
            local item
            fd -L0 | fzf --read0 --height 40% --reverse --prompt "> $dir" -q "$query" -m --print0 |
                while read -rd $'\0' item; do
                    echo -n "$dir${(q)item} "
                done
            REPORTTIME=5
        }

        fzf-file-widget() {
            local tokens=(${(z)LBUFFER})
            [[ ${LBUFFER[-1]} =~ '\s' ]] && tokens+=
            local results=$(fzf-file-widget-helper ${tokens[-1]})
            [[ -n $results ]] && LBUFFER=${LBUFFER:0:${#LBUFFER}-${#tokens[-1]}}$results
            zle reset-prompt
        }

        fzf-cd-widget() {
            unset REPORTTIME
            cd -- "$(fd -L0td | fzf --read0 --height 40% --reverse --prompt 'cd ')"
            REPORTTIME=5
            zle reset-prompt
        }
    fi
fi
