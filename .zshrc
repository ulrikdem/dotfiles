zstyle ':completion:*' menu select
zstyle ':completion:*:descriptions' format '%8F%d:%f'

type dircolors >/dev/null && eval $(dircolors ~/.config/dir_colors)
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*:*:git-*:*:files' command '-git-files-wrapper'
function git-files-wrapper {
    if [[ "$1 $2" = 'git ls-files' ]]; then
        shift 2
        git ls-files --directory "$@"
    else
        "$@"
    fi
}

if [[ -d ~/.dotfiles.git ]]; then
    function dotfiles {
        (($#)) || set zsh
        GIT_DIR=~/.dotfiles.git GIT_WORK_TREE=~ "$@"
    }
    alias dotfiles='dotfiles '
    compdef 'dotfiles _precommand' dotfiles 2>/dev/null
fi

type abduco >/dev/null && alias abduco="abduco -e '^H'"
type diff >/dev/null && alias diff='diff --color=auto'
type gcc >/dev/null && alias gcc='gcc -std=c17 -Wall -Wextra -Wconversion'
type g++ >/dev/null && alias g++='g++ -std=c++20 -Wall -Wextra -Wconversion'
type git >/dev/null && alias g=git
type nvim >/dev/null && alias vi=nvim vim=nvim
type nvr >/dev/null && [[ -n $NVIM_LISTEN_ADDRESS ]] && alias vi=nvr vim=nvr
type ranger >/dev/null && alias r=ranger
type ssh >/dev/null && alias ssh='TERM=xterm-256color ssh'

setopt no_bg_nice
setopt chase_links
setopt no_check_jobs
setopt glob_star_short
setopt list_packed
setopt no_list_types
setopt pushd_silent
setopt rc_expand_param
setopt rc_quotes

stty -ixon

if declare -f isgrml >/dev/null; then
    zstyle :prompt:grml:left:setup items user at host fullpath vcs venv rc newline arrow
    zstyle :prompt:grml:right:setup use-rprompt false

    zstyle :prompt:grml:left:items:at pre %F{8}
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
    function prompt-venv {
        REPLY=${VIRTUAL_ENV+%8F\[%F\{blue\}venv%8F\] %f}
    }

    grml_theme_add_token arrow '%F{blue}» %f'
    PS2='%F{blue}» %f'

    [[ -n $key[BackTab] ]] && bindkey -- $key[BackTab] reverse-menu-complete

    autoload up-line-or-beginning-search down-line-or-beginning-search
    zle -N up-line-or-beginning-search
    zle -N down-line-or-beginning-search
    [[ -n $key[Up] ]] && bindkey -- $key[Up] up-line-or-beginning-search
    [[ -n $key[Down] ]] && bindkey -- $key[Down] down-line-or-beginning-search

    abk[LC]='--color=always |& less -R'
fi

zle_highlight=(suffix:fg=8)

if [[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    ZSH_HIGHLIGHT_HIGHLIGHTERS+=(brackets)
    ZSH_HIGHLIGHT_STYLES+=(
        path_prefix none
        path bold
        assign fg=cyan
        unknown-token fg=red
        bracket-error fg=red
        bracket-level-1 fg=8
        comment fg=8
    )
    noglob unset ZSH_HIGHLIGHT_STYLES[precommand] ZSH_HIGHLIGHT_STYLES[bracket-level-{2..5}]
fi

if [[ -f /usr/share/fzf/key-bindings.zsh ]]; then
    source /usr/share/fzf/key-bindings.zsh

    if type fd >/dev/null; then
        function fzf-file-widget {
            local words=(${(z)LBUFFER})
            [[ $LBUFFER =~ '\s$' ]] && local word= || local word=$words[-1]
            local query=${word##*/}
            local dir=${word:0:$#word-$#query}
            while [[ -n $dir && ! -d ${(Q)${~dir}} ]]; do
                query=$dir:t/$query
                dir=${word:0:$#word-$#query}
            done
            local results=$(
                cd -- ${(Q)${~dir}:-.}
                unset REPORTTIME
                fd -L0 --strip-cwd-prefix |
                    fzf --read0 --height 40% --reverse --prompt "${(Q)dir:-./}" -q "$query" --bind ctrl-z:ignore -m --print0 |
                    while read -rd $'\0' item; do
                        echo -nE - "$dir${(q)item} "
                    done
            )
            [[ -n $results ]] && LBUFFER=${LBUFFER:0:$#LBUFFER-$#word}$results
            zle redisplay
        }

        function fzf-cd-widget {
            local dir=$(
                unset REPORTTIME
                fd -L0td --strip-cwd-prefix | fzf --read0 --height 40% --reverse --prompt 'cd ' --bind ctrl-z:ignore
            )
            zle redisplay
            if [[ -n $dir ]]; then
                zle push-line
                BUFFER="cd ${(q)dir}"
                zle accept-line
            fi
        }
    fi
fi
