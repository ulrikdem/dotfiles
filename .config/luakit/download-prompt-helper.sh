#!/bin/bash
query=
while true; do
    readarray -d '' result < <(cat <(printf '..\0') <(fd --max-depth 1 --follow --type d --print0) |
        fzf --read0 --reverse --prompt "${PWD%/}/" --query "$query" --expect ctrl-o,tab,shift-tab --print-query --print0 --no-clear)
    ((!${#result[@]})) && exit
    query=
    case ${result[1]} in
        ctrl-o)
            mktemp -t "luakit.XXXXXXXX.$1"
            exit;;
        tab)
            [[ -n ${result[2]} ]] && cd -- "${result[2]}" || cd -- "${result[0]}" 2>/dev/null || query=${result[0]};;
        shift-tab)
            [[ -n ${result[0]} ]] && mkdir -p -- "${result[0]}" && cd -- "${result[0]}";;
        *)
            [[ -n ${result[0]} ]] && echo "${PWD%/}/${result[0]}" || echo "${PWD%/}/$1"
            exit;;
    esac
done >$2
