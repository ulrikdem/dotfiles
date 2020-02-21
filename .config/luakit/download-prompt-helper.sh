#!/bin/bash
query=
while true; do
    readarray -t result < <(ls --color | fzf --ansi --color hl:3,hl+:3,fg+:-1 --no-clear --reverse \
        --prompt "${PWD%/}/" --query "$query" --print-query --expect ctrl-p,tab,ctrl-n,ctrl-o)
    ((!${#result[@]})) && exit
    query=
    case ${result[1]} in
        ctrl-p)
            cd ..
            query=${result[0]};;
        tab)
            [[ -n ${result[2]} ]] && name=${result[2]} || name=${result[0]}
            cd -- "$name" 2>/dev/null || query=$name;;
        ctrl-n)
            [[ -n ${result[0]} ]] && mkdir -p -- "${result[0]}" && cd -- "${result[0]}";;
        ctrl-o)
            mktemp -t "luakit.XXXXXXXX.$1"
            exit;;
        *)
            [[ -n ${result[0]} ]] && echo "${PWD%/}/${result[0]}" || echo "${PWD%/}/$1"
            exit;;
    esac
done >$2
