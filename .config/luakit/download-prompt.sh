#!/bin/bash
temp=$(mktemp -t luakit.XXXXXXXX)
termite --name download-prompt -t "Download $1" -e "'$(dirname "$0")/download-prompt-helper.sh' '$1' $temp" 2>/dev/null
file=$(<$temp)
rm $temp
[[ -n $file ]] && echo -n "$file" || exit 1
