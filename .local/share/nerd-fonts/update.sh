#!/bin/bash
cd "$(dirname "$0")"
url=https://raw.githubusercontent.com/ryanoasis/nerd-fonts/master/bin/scripts/lib
wget -nv -O i_all.sh $url/i_all.sh
source i_all.sh
for i in "${sets[@]}"; do
    wget -nv -O i_$i.sh $url/i_$i.sh
done
