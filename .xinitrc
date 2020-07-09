#!/bin/bash

~/.config/xplugrc xinit
xplugd

xrandr --listproviders | grep -q NVIDIA && gpu_icon=' ﯎ <fc=gray25>│</fc>'
xprop -root -f GPU_ICON 8u -set GPU_ICON "$gpu_icon"

export GDK_SCALE=$(cat ~/.config/dpi-scale || echo 1)
export GDK_DPI_SCALE=$(bc <<<"scale=2; 1/$GDK_SCALE")
echo "Xft.dpi: $((96*GDK_SCALE))" | xrdb -merge

xset s off
xset dpms 0 0 0
xss-lock -- sh -c 'xset dpms 5; i3lock -nec 000000 && killall -q i3lock; xset dpms 0' &

feh --no-fehbg --bg-fill ~/.config/wallpaper.jpg
picom -b

export _JAVA_AWT_WM_NONREPARENTING=1
mkdir -p ~/.cache/xmonad ~/.local/share/xmonad
exec xmonad
