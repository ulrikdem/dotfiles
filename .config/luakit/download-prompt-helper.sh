#!/bin/bash
eval $(dircolors ~/.config/dir_colors)
query=
while true; do
    readarray -t result < <(cat <(ls --color) | fzf --ansi --color hl:3,hl+:3,fg+:-1,header:8 --header "$1" --reverse \
        --no-clear --prompt "${PWD%/}/" --query "$query" --print-query --expect ctrl-p,tab,shift-tab,ctrl-o)
    ((!${#result[@]})) && exit
    query=
    case ${result[1]} in
        ctrl-p)
            cd ..
            query=${result[0]};;
        tab)
            [[ -n ${result[2]} ]] && name=${result[2]} || name=${result[0]}
            cd -- "$name" 2>/dev/null || query=$name;;
        shift-tab)
            [[ -n ${result[0]} ]] && mkdir -p -- "${result[0]}" && cd -- "${result[0]}";;
        ctrl-o)
            mktemp -t "luakit.XXXXXXXX.$1"
            exit;;
        *)
            [[ -n ${result[0]} ]] && echo "${PWD%/}/${result[0]}" || echo "${PWD%/}/$1"
            exit;;
    esac
done >"$2"
