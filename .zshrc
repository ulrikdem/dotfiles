# Options {{{1

# Changing Directories
setopt auto_cd
setopt auto_pushd
setopt cd_silent
setopt chase_links
setopt pushd_ignore_dups
setopt pushd_silent

# Completion
setopt complete_in_word
setopt list_packed
setopt no_list_types

# Expansion and Globbing
setopt extended_glob
setopt glob_star_short
setopt magic_equal_subst
setopt rc_expand_param

# History
setopt extended_history
setopt hist_ignore_space
setopt hist_lex_words
setopt share_history

# Input/Output
setopt interactive_comments
setopt rc_quotes

# Job Control
setopt auto_continue
setopt no_bg_nice
setopt no_check_running_jobs
setopt no_hup

# Prompting
setopt prompt_subst
setopt transient_rprompt

# ZLE
setopt no_beep
setopt combining_chars

# Parameters and Modules {{{1

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=$HISTSIZE

KEYTIMEOUT=1
LISTMAX=0
REPORTTIME=5

zmodload zsh/complist
zmodload zsh/parameter
zmodload zsh/terminfo
zmodload zsh/zutil

# Prompt and Title {{{1

PROMPT='%F{blue}%B%n%b%8F@%F{blue}%B%m%f %~%b$vcs_info_msg_0_${VIRTUAL_ENV+ %8F[%F{blue\}venv%8F]%f}%(?.. %F{red}%?%f)
%F{blue}»%f '
PROMPT2='%F{blue}»%f '

autoload -U vcs_info
zstyle ':vcs_info:*' formats ' %8F[%F{blue}%b%c%u%8F]%f'
zstyle ':vcs_info:*' actionformats ' %8F[%F{blue}%b%8F:%F{magenta}%a%c%u%8F]%f'
zstyle ':vcs_info:*' branchformat '%b%8F:%F{blue}%r'
zstyle ':vcs_info:*' stagedstr '%8F:%F{green}S'
zstyle ':vcs_info:*' unstagedstr '%8F:%F{red}U'
zstyle ':vcs_info:*' check-for-changes true

function precmd {
    vcs_info
    print -Pn '\e]2;%n@%m: %~\a'
}
function preexec {
    print -Pn '\e]2;%n@%m: '
    printf '%s\a' "$1"
}

VIRTUAL_ENV_DISABLE_PROMPT=1

RPROMPT=
RPROMPT2=
ZLE_RPROMPT_INDENT=0

# Key Bindings {{{1

bindkey -e

autoload -U select-word-style
select-word-style shell

function bindkeymaps {
    local key=$1 widget=$2 keymap
    shift 2
    if [[ -n $key ]]; then
        if ! zle -la "$widget"; then
            (($+functions[$widget])) || autoload -U "$widget"
            zle -N "$widget"
        fi
        for keymap; do
            bindkey -M "$keymap" -- "$key" "$widget"
        done
    fi
}

bindkeymaps "$terminfo[kcuu1]" up-line-or-beginning-search main
bindkeymaps "$terminfo[kcud1]" down-line-or-beginning-search main

bindkeymaps "$terminfo[khome]" beginning-of-line main vicmd
bindkeymaps "$terminfo[kend]" end-of-line main vicmd

bindkeymaps "$terminfo[kLFT5]" backward-word main vicmd
bindkeymaps "$terminfo[kRIT5]" forward-word main vicmd

bindkeymaps "$terminfo[kdch1]" delete-char main vicmd

bindkeymaps '\t' complete-word main
bindkeymaps "$terminfo[kcbt]" reverse-menu-complete main

autoload -U bracketed-paste-url-magic
zle -N bracketed-paste bracketed-paste-url-magic

bindkeymaps '\e' vi-cmd-mode main
bindkeymaps "S'" quote-region visual

for key in {a,i}{\',\",\`}; do
    bindkeymaps $key select-quoted viopp visual
done
for key in {a,i}${(s..):-'()[]{}<>bB'}; do
    bindkeymaps $key select-bracketed viopp visual
done
bindkeymaps 'a^W' select-word-match viopp visual
bindkeymaps 'i^W' select-word-match viopp visual
unset key

bindkeymaps '^A' incarg vicmd
function decarg { NUMERIC=$((-${NUMERIC:-1})) incarg }
bindkeymaps '^X' decarg vicmd

function zle-line-pre-redraw {
    if [[ $KEYMAP = vicmd ]]; then
        case $REGION_ACTIVE in
            0) RPROMPT=$'%0F%K{blue} NORMAL %k%f%{\e[2 q%}';;
            1) RPROMPT=$'%0F%K{magenta} VISUAL %k%f%{\e[2 q%}';;
            2) RPROMPT=$'%0F%K{magenta} V-LINE %k%f%{\e[2 q%}';;
        esac
    elif [[ $ZLE_STATE = *overwrite* ]]; then
        RPROMPT=$'%0F%K{red} REPLACE %k%f%{\e[4 q%}'
    else
        RPROMPT=$'%0F%K{green} INSERT %k%f%{\e[6 q%}'
    fi
    if [[ $RPROMPT != $RPROMPT2 ]]; then
        RPROMPT2=$RPROMPT
        zle reset-prompt
    fi
}
zle -N zle-line-pre-redraw

function zle-line-init {
    zle-line-pre-redraw
    (($+terminfo[smkx])) && echoti smkx
}
function zle-line-finish {
    (($+terminfo[rmkx])) && echoti rmkx
    echo -n '\e[2 q'
}
zle -N zle-line-init
zle -N zle-line-finish

stty -ixon

# Completion {{{1

autoload -U compinit
compinit -d ~/.cache/zcompdump

zstyle ':completion:*' use-cache true
zstyle ':completion:*' cache-path ~/.cache/zcompcache

zstyle ':completion:*' menu select
zstyle ':completion:*' select-scroll -1
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

zstyle ':completion:*' completer _extensions _complete
zstyle ':completion:*' ignore-parents pwd
zstyle ':completion:*' list-suffixes true

zstyle ':completion:*' rehash true
zstyle ':completion:*:processes' command 'ps xo pid:8,args'
zstyle ':completion:*:jobs' numbers true

zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.^1' insert-sections true

zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%8F%d:%f'
zstyle ':completion:*:warnings' format '%8Fno matches for %d%f'
zstyle ':completion:*:messages' format '%8F%d%f'

if (($+commands[dircolors])) && [[ -f ~/.config/dir_colors ]]; then
    eval $(dircolors ~/.config/dir_colors)
    zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi

zle -C external-pwds complete-word _generic
bindkeymaps '^Xo' external-pwds main
zstyle ':completion:external-pwds:*' completer _external_pwds

zstyle ':completion:*:*:git-*:*:files' command '-git-files-wrapper'
function git-files-wrapper {
    if [[ "$1 $2" = 'git ls-files' ]]; then
        shift 2
        git ls-files --directory "$@"
    else
        "$@"
    fi
}

# Aliases and Functions {{{1

alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -lha'

(($+commands[diff])) && alias diff='diff --color'
(($+commands[grep])) && alias grep='grep --color'
(($+commands[ssh])) && alias ssh='TERM=xterm-256color ssh'
(($+commands[abduco])) && alias abduco="abduco -e '^H'"

(($+commands[nvim])) && alias vi=nvim vim=nvim
(($+commands[nvr] && $+NVIM_LISTEN_ADDRESS)) && alias vi=nvr vim=nvr
(($+commands[ranger])) && alias r=ranger
(($+commands[git])) && alias g=git

if [[ -d ~/.dotfiles.git ]]; then
    function dotfiles {
        (($#)) || set zsh
        GIT_DIR=~/.dotfiles.git GIT_WORK_TREE=~ "$@"
    }
    alias dotfiles='dotfiles '
    compdef 'dotfiles _precommand' dotfiles
fi

(($+commands[gcc])) && alias gcc='gcc -std=c17 -Wall -Wextra -Wconversion'
(($+commands[g++])) && alias g++='g++ -std=c++20 -Wall -Wextra -Wconversion'

alias cdtmp='cd -- "$(mktemp -td cdtmp.XXXXXXXX)"'
alias rmcdir='cd .. && rmdir -- "$OLDPWD"'

autoload -U zargs
autoload -U zmv

(($+aliases[run-help])) && unalias run-help
autoload -U run-help{,-git,-ip,-openssl,-p4,-sudo,-svk,-svn}

# Syntax Highlighting {{{1

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

zle_highlight=(suffix:fg=8 region:fg=8,standout paste:none)

# FZF {{{1

if [[ -f /usr/share/fzf/key-bindings.zsh ]]; then
    source /usr/share/fzf/key-bindings.zsh

    if (($+commands[fd])); then
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

# Local Configuration {{{1

if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

# vim: foldmethod=marker
