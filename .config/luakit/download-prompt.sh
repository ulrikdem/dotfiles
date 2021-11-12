#!/bin/bash
temp=$(mktemp -t luakit.XXXXXXXX)
alacritty --class xmonad-custom-float -t "Download $1" \
    -e "$(dirname "$0")/download-prompt-helper.sh" "$1" "$temp"
file=$(<"$temp")
rm -- "$temp"
[[ -n $file ]] && echo -n "$file" || exit 1
