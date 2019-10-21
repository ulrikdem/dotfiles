#!/bin/bash
cd -- "$1/adblock"
for file in easylist.txt easyprivacy.txt; do
    if [[ ! -f $file ]] || (($(date +%s) - $(stat -c %Y $file) > 5 * 24 * 60 * 60)); then
        temp=$(mktemp -t luakit.XXXXXXXX)
        wget -qO $temp https://easylist.to/easylist/$file && mv $temp $file || rm $temp
    fi
done
