#!/bin/bash

xrandr --auto
xrandr --listproviders | grep -q NVIDIA && gpu_icon=' '
xprop -root -f GPU_ICON 8u -set GPU_ICON "$gpu_icon"

xmodmap -e 'remove Lock = Caps_Lock' -e 'keysym Caps_Lock = Control_L' -e 'add Control = Control_L'
numlockx

xset s off
xset dpms 0 0 0
xss-lock -- sh -c '/usr/bin/i3lock -nc 000000 && killall -q i3lock' &

feh --no-fehbg --bg-fill ~/.config/wallpaper.jpg
picom -b

export _JAVA_AWT_WM_NONREPARENTING=1
mkdir -p ~/.cache/xmonad ~/.local/share/xmonad
exec xmonad
