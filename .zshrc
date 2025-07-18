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
HISTSIZE=100000
SAVEHIST=$HISTSIZE

KEYTIMEOUT=1
LISTMAX=0
REPORTTIME=5

zmodload zsh/complist
zmodload zsh/parameter
zmodload zsh/terminfo
zmodload zsh/zutil

# Prompt and Title {{{1

[[ $TERM = linux ]] && fancy_prompt= || fancy_prompt=1
PROMPT=${fancy_prompt:+%K{#404040} }${SSH_TTY+%B%m%b%8F:%f}%B%~%b${fancy_prompt:+ %k%F{#404040}%f}'$vcs_info_msg_0_${VIRTUAL_ENV+ %8F[%fvenv%8F]%f}'${SANDBOX+ %8F[%fsb%8F]%f}'%(?.. %F{red}%?%f)
%8F»%f '
PROMPT2='%8F»%f '
unset fancy_prompt

autoload -U vcs_info
zstyle ':vcs_info:*' formats ' %8F[%f%b%c%u%8F]%f'
zstyle ':vcs_info:*' actionformats ' %8F[%f%b%8F:%f%a%c%u%8F]%f'
zstyle ':vcs_info:*' branchformat '%b%8F:%f%r'
zstyle ':vcs_info:*' stagedstr '%8F:%F{green}S'
zstyle ':vcs_info:*' unstagedstr '%8F:%F{red}U'
zstyle ':vcs_info:*' check-for-changes true

function precmd {
    vcs_info
    print -Pn "\e]2;${SSH_TTY+%m:}%~\a"
}
function preexec {
    printf '\e]2;%s\a' "$1"
}

VIRTUAL_ENV_DISABLE_PROMPT=1

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

bindkeymaps "$terminfo[khome]" beginning-of-line main vicmd viopp visual
bindkeymaps "$terminfo[kend]" end-of-line main vicmd viopp visual

bindkeymaps "$terminfo[kLFT5]" backward-word main vicmd viopp visual
bindkeymaps "$terminfo[kRIT5]" forward-word main vicmd viopp visual

WORDCHARS='"#$%''*+-.?\_'
bindkeymaps "$terminfo[kLFT]" .backward-word main vicmd viopp visual
bindkeymaps "$terminfo[kRIT]" .forward-word main vicmd viopp visual
bindkeymaps '^W' .backward-kill-word main

bindkeymaps "$terminfo[kdch1]" delete-char main vicmd

bindkeymaps "$terminfo[kich1]" vi-insert vicmd
bindkeymaps "$terminfo[kich1]" overwrite-mode main

bindkeymaps '\t' complete-word main
bindkeymaps "$terminfo[kcbt]" reverse-menu-complete main

bindkeymaps '\e^_' copy-earlier-word main

bindkeymaps '\eo' accept-and-infer-next-history main

function accept-history { print -rS "$BUFFER"; zle send-break }
bindkeymaps '\eH' accept-history main vicmd

bindkeymaps '^Q' push-line-or-edit main vicmd

bindkeymaps '\ee' edit-command-line main vicmd

bindkeymaps '^V' vi-quoted-insert main
bindkeymaps '\ev' insert-unicode-char main

function expand-dots { [[ $LBUFFER = (^*[[:WORD:]]).. ]] && LBUFFER+=/.. || LBUFFER+=. }
bindkeymaps . expand-dots main

bindkeymaps ' ' magic-space main

bindkeymaps '^Xp' expand-absolute-path main

function inplace-mkdir {
    local words=(${(z)LBUFFER})
    zle -M "$(mkdir -pv -- ${(Q)${~words[-1]}} 2>&1)"
}
bindkeymaps '\em' inplace-mkdir main

autoload -U bracketed-paste-url-magic
zle -N bracketed-paste bracketed-paste-url-magic

bindkeymaps '\e' vi-cmd-mode main

bindkeymaps Y vi-yank-eol vicmd

bindkeymaps "S'" quote-region visual

for key in {a,i}{\',\",\`}; do
    bindkeymaps $key select-quoted viopp visual
done
for key in {a,i}${(s..):-'()[]{}<>bB'}; do
    bindkeymaps $key select-bracketed viopp visual
done
unset key

bindkeymaps 'a^W' select-word-match viopp visual
bindkeymaps 'i^W' select-word-match viopp visual

bindkeymaps '^A' incarg vicmd
function decarg { NUMERIC=$((-${NUMERIC:-1})) incarg }
bindkeymaps '^X' decarg vicmd

function smkx {
    (($+terminfo[smkx])) && echoti smkx
}
function rmkx {
    (($+terminfo[rmkx])) && echoti rmkx
}
autoload -U add-zle-hook-widget
add-zle-hook-widget line-init smkx
add-zle-hook-widget line-finish rmkx

stty -ixon

# Completion {{{1

autoload -U compinit
compinit -d ~/.cache/zcompdump

zstyle ':completion:*' use-cache true
zstyle ':completion:*' cache-path ~/.cache/zcompcache

zstyle ':completion:*' menu select
zstyle ':completion:*' select-scroll -1
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

zstyle ':completion:*' completer _extensions _complete _files
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

function _sb {
    local curcontext=$curcontext state state_descr line
    local -A opt_args
    _arguments -s -C : -{n,x,u,R,W} '*-'{r,w}'+: :->mount' '*-e: :->var' '*-'{b,B}'+: :->dbus' \
        '*-s+:namespace:(cgroup ipc net pid user uts)' '*::: :->command' && return
    case $state in
        mount)
            if compset -P : || compset -S 1 '=*'; then
                _files
            else
                compset -P '*='
                if [[ $PREFIX$SUFFIX = *:* ]]; then
                    _dir_list
                else
                    _files -r '/=: \t\n\-'
                fi
            fi;;
        var)
            if ! compset -P 1 '*='; then
                local arg
                compset -S '=*' || arg=-qS=
                _parameters -g '*export*' $arg
            else
                _default
            fi;;
        dbus)
            local name names=(
                $(qdbus6 org.freedesktop.DBus /org/freedesktop/DBus ListNames | grep '^[^:]')
                $(qdbus6 org.freedesktop.DBus /org/freedesktop/DBus ListActivatableNames)
            ) 2>/dev/null
            local completions=($names)
            for name in $names; do
                while [[ $name = *.* ]]; do
                    name=${name%.*}
                    completions+=("$name.*")
                done
            done
            _describe 'dbus name' completions;;
        command)
            if [[ $words[1] = -- ]]; then
                _bwrap
            else
                _normal -p sb
            fi;;
        *)
            return 1;;
    esac
}

compdef _sb sb
compdef "_arguments ':directory:_files -/' '*::: :{_normal -p venv}'" venv
compdef _precommand nv
compdef _precommand vpn
compdef "_arguments ':subcommand:(toggle target undo edit bar)'" work

# Aliases and Functions {{{1

[[ $TERM = alacritty ]] && alias ls='ls --color=auto --hyperlink=auto' || alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -lha'

(($+commands[diff])) && alias diff='diff --color'
(($+commands[grep])) && alias grep='grep --color'
(($+commands[ssh])) && alias ssh='TERM=xterm-256color ssh'

(($+commands[bsdtar])) && alias tar=bsdtar
(($+commands[git])) && alias g=git
(($+commands[ipython])) && alias ipy=ipython
(($+commands[nvim])) && alias vi=nvim
(($+commands[nvim] && $+NVIM)) && alias vi='nvim --server "$NVIM" --remote'
(($+commands[pacman])) && alias pm=pacman spm='sudo pacman'
(($+commands[ranger])) && alias r=ranger
(($+commands[xdg-open])) && alias o=xdg-open

if [[ -d ~/.dotfiles.git ]]; then
    function conf {
        (($#)) || set zsh
        GIT_DIR=~/.dotfiles.git GIT_WORK_TREE=~ "$@"
    }
    alias conf='conf '
    compdef 'conf _precommand' conf
fi

(($+commands[gcc])) && alias gcc='gcc -std=c17 -Wall -Wextra -Wconversion'
(($+commands[g++])) && alias g++='g++ -std=c++20 -Wall -Wextra -Wconversion'

(($+commands[nsenter])) && alias sbenter='sudo nsenter -aew -S follow -G follow -t'

(($+commands[sudo])) && alias sudo='sudo '
(($+commands[xargs])) && alias xargs='xargs '

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
        autodirectory bold
        assign fg=cyan
        unknown-token fg=red
        bracket-error fg=red
        bracket-level-1 fg=8
        comment fg=8
    )
    noglob unset ZSH_HIGHLIGHT_STYLES[precommand] ZSH_HIGHLIGHT_STYLES[bracket-level-{2..5}]
fi

zle_highlight=(suffix:fg=8 region:fg=8,standout paste:none)

# Vi Mode Indicator {{{1

RPROMPT='${vi_mode:+%2F-- $vi_mode --%f}'
RPROMPT2=$RPROMPT
ZLE_RPROMPT_INDENT=0

function vi_mode_update {
    local old_mode=$vi_mode old_cursor=$vi_mode_cursor
    if [[ $KEYMAP = vicmd ]]; then
        case $REGION_ACTIVE in
            0) vi_mode=NORMAL;;
            1) vi_mode=VISUAL;;
            2) vi_mode='VISUAL LINE';;
        esac
        vi_mode_cursor=2
    elif [[ $ZLE_STATE = *overwrite* ]]; then
        vi_mode=REPLACE vi_mode_cursor=4
    else
        vi_mode= vi_mode_cursor=6
    fi
    [[ $vi_mode != $old_mode ]] && zle reset-prompt
    [[ $vi_mode_cursor != $old_cursor ]] && echo -n "\e[$vi_mode_cursor q"
}
add-zle-hook-widget line-init vi_mode_update
add-zle-hook-widget line-pre-redraw vi_mode_update

function vi_mode_reset {
    vi_mode_cursor=2
    echo -n "\e[$vi_mode_cursor q"
}
add-zle-hook-widget line-finish vi_mode_reset

# FZF {{{1

if (($+commands[fzf])); then
    if (($+commands[fd])); then
        function fzf-file-widget {
            local words=(${(z)LBUFFER})
            [[ $LBUFFER =~ '\s$' ]] && local word= || local word=$words[-1]
            local dir=${word:+${word%/}/} query=
            while [[ -n $dir && ! -d ${(Q)${~dir}} ]]; do
                dir=${dir%%[^/]#/}
                query=${word:$#dir}
            done
            local results=$(
                cd -- ${(Q)${~dir}:-.}
                unset REPORTTIME
                fd -L0 --strip-cwd-prefix |
                    fzf --read0 --height 11 --reverse --prompt "${(Q)dir:-./}" -q "$query" -m --print0 |
                    while read -rd '' item; do
                        echo -nE - "$dir${(q)item} "
                    done
            )
            [[ -n $results ]] && LBUFFER=${LBUFFER:0:$#LBUFFER-$#word}$results
            zle redisplay
        }
        bindkeymaps '^T' fzf-file-widget main
    fi

    if (($+commands[awk])); then
        function fzf-history-widget {
            local result
            result=$(
                printf '%s\0' "${history[@]}" |
                    awk -v RS='\0' -v ORS='\0' '{ if (!seen[$0]++) print $0 }' |
                    fzf --read0 --height 11 --prompt 'history: ' -q "$LBUFFER" --scheme history
            ) && LBUFFER=$result
            zle redisplay
        }
        bindkeymaps '^R' fzf-history-widget main
    fi

    if (($+commands[rg] && $+commands[nvim] && $+commands[jq])); then
        function fzf-grep-widget {
            local results=$(
                unset REPORTTIME
                fzf --height 11 --reverse --prompt 'grep: ' --bind change:top+reload:'rg --json -Se {q} | nvim -l ~/.config/nvim/scripts/igrep_format.lua $COLUMNS' --with-nth 2.. --delimiter '\t' --ansi --disabled -m |
                    cut -f 1 | jq --raw-output0 .data.path.text |
                    while read -rd '' item; do
                        echo -nE - "${(q)item} "
                    done
            )
            LBUFFER=$LBUFFER$results
            zle redisplay
        }
        bindkeymaps '\eg' fzf-grep-widget main
    fi
fi

# Local Configuration {{{1

if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

# vim: foldmethod=marker
