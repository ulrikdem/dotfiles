zstyle ':completion:*' menu select
zstyle ':completion:*:descriptions' format '%8F%d:%f'

type -f dircolors >/dev/null && eval $(dircolors ~/.config/dir_colors)
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*:*:git-*:*:files' command '-git-files-wrapper'
git-files-wrapper() {
    if [[ "$1 $2" = 'git ls-files' ]]; then
        shift 2
        git ls-files --directory "$@"
    else
        "$@"
    fi
}

if [[ -d ~/.dotfiles.git ]]; then
    dotfiles() {
        (($#)) || set zsh
        GIT_DIR=~/.dotfiles.git GIT_WORK_TREE=~ "$@"
    }
    typeset -f compdef >/dev/null && compdef 'dotfiles _precommand' dotfiles
fi

type -p abduco >/dev/null && alias abduco="abduco -e '^H'"
type -p diff >/dev/null && alias diff='diff --color=auto'
type -p gcc >/dev/null && alias gcc='gcc -std=c17 -Wall -Wextra -Wconversion'
type -p g++ >/dev/null && alias g++='g++ -std=c++20 -Wall -Wextra -Wconversion'
type -p nvim >/dev/null && alias vim=nvim
type -p nvr >/dev/null && [[ -n $NVIM_LISTEN_ADDRESS ]] && alias vim=nvr
type -p ssh >/dev/null && alias ssh='TERM=xterm-256color ssh'

setopt no_bg_nice
setopt chase_links
setopt no_check_jobs
setopt list_packed
setopt no_list_types
setopt rc_quotes

stty -ixon

zle_highlight=(suffix:fg=8)

if [[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    ZSH_HIGHLIGHT_HIGHLIGHTERS+=(brackets)
    ZSH_HIGHLIGHT_STYLES+=(
        default fg=15
        assign fg=cyan
        unknown-token fg=red
        bracket-error fg=red
        bracket-level-1 fg=8
        comment fg=8
    )
    noglob unset ZSH_HIGHLIGHT_STYLES[path] ZSH_HIGHLIGHT_STYLES[precommand] \
        ZSH_HIGHLIGHT_STYLES[bracket-level-{2..5}]
fi

[[ -f /etc/profile.d/vte.sh ]] && source /etc/profile.d/vte.sh

if typeset -f isgrml >/dev/null; then
    zstyle :prompt:grml:left:setup items user at host fullpath vcs venv rc newline arrow
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

    grml_theme_add_token venv -f prompt-venv
    prompt-venv() {
        REPLY=${VIRTUAL_ENV+%8F\[%F\{blue\}venv%8F\] %f}
    }

    grml_theme_add_token arrow '%F{blue}» %f'
    PS2='%8F%_ %F{blue}» %f'

    bindkey '^P' history-beginning-search-backward-end
    bindkey '^N' history-beginning-search-forward-end

    bindkey -s '^S' '^X.'
    abk[LC]='--color=always |& less -r'
fi

if [[ -f ~/.local/share/nvim/plugged/fzf/shell/key-bindings.zsh ]]; then
    source ~/.local/share/nvim/plugged/fzf/shell/key-bindings.zsh

    if type fzf >/dev/null && type fd >/dev/null; then
        fzf-file-widget() {
            local words=(${(z)LBUFFER})
            [[ $LBUFFER =~ '\s$' ]] && local word= || local word=$words[-1]
            local query=${word##*/}
            local dir=${word:0:$#word-$#query}
            while [[ -n $dir && ! -d $~dir ]]; do
                query=${${dir%/}##*/}/$query
                dir=${word:0:$#word-$#query}
            done
            local results=$(
                cd -- ${~dir:-.}
                unset REPORTTIME
                fd -L0 | fzf --read0 --height 40% --reverse --prompt ${dir:-./} -q "$query" -m --print0 |
                    while read -rd $'\0' item; do
                        echo -nE "$dir${(q)item} "
                    done
            )
            [[ -n $results ]] && LBUFFER=${LBUFFER:0:$#LBUFFER-$#word}$results
            zle reset-prompt
        }

        fzf-cd-widget() {
            cd -- "$(unset REPORTTIME; fd -L0td | fzf --read0 --height 40% --reverse --prompt 'cd ')"
            zle fzf-redraw-prompt
        }
    fi
fi
