alias rmcdir-r='cd ..; rm -r $OLDPWD'
type diff >/dev/null && alias diff='diff --color=auto'
type gcc >/dev/null && alias gcc='gcc -Wall -Wextra'
type g++ >/dev/null && alias g++='g++ -Wall -Wextra'
type git >/dev/null && [[ -d ~/.dotfiles.git ]] && alias dotfiles-git='git --git-dir $HOME/.dotfiles.git --work-tree $HOME -c status.showUntrackedFiles=no'
type nvim >/dev/null && alias vim=nvim
type ssh >/dev/null && alias ssh='TERM=xterm-256color ssh'
type udevil >/dev/null && alias pmount='udevil mount'
type udevil >/dev/null && alias pumount='udevil umount'
[[ -v abk ]] && abk[LC]='--color=always |& less -r'

bindkey '^N' history-beginning-search-forward-end
bindkey '^P' history-beginning-search-backward-end

stty -ixon

LS_COLORS=${LS_COLORS/ow=34;42/ow=01;34}
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu select
zstyle ':prompt:grml:right:setup' use-rprompt false

setopt NO_CHECK_JOBS

[[ -r /etc/profile.d/vte.sh ]] && . /etc/profile.d/vte.sh

if [[ -r /usr/share/fzf/key-bindings.zsh ]]; then
    . /usr/share/fzf/key-bindings.zsh

    if type fd >/dev/null; then
        fzf-file-widget-helper() {
            local query=${1##*/}
            local dir=${1:0:${#1}-${#query}}
            while [[ -n $dir && ! -d $dir ]]; do
                dir=${dir%/}
                query=${dir##*/}/$query
                dir=${1:0:${#1}-${#query}}
            done
            cd "$dir"
            unset REPORTTIME
            local item
            fd -L0 | fzf --read0 --height 40% --reverse --prompt "> $dir" -q "$query" -m --print0 | while read -rd $'\0' item; do
                echo -n "$dir${(q)item} "
            done
            REPORTTIME=5
        }

        fzf-file-widget() {
            local tokens=(${(z)LBUFFER})
            [[ ${LBUFFER[-1]} =~ '\s' ]] && tokens+=
            local results=$(fzf-file-widget-helper ${tokens[-1]})
            if [[ -n $results ]]; then
                LBUFFER=${LBUFFER:0:${#LBUFFER}-${#tokens[-1]}}$results
            fi
            zle reset-prompt
        }

        fzf-cd-widget() {
            unset REPORTTIME
            cd "$(fd -L0td | fzf --read0 --height 40% --reverse --prompt 'cd ')"
            REPORTTIME=5
            zle reset-prompt
        }
    fi
fi
