#!/bin/bash
xrandr --auto
if xrandr --listproviders | grep -q NVIDIA; then
    xprop -root -f gpu 8s -set gpu ' '
else
    xprop -root -f gpu 8s -set gpu ''
fi
feh --no-fehbg --bg-fill ~/.config/wallpaper.jpg
compton -b
xmodmap -e 'remove Lock = Caps_Lock' -e 'keysym Caps_Lock = Control_L' -e 'add Control = Control_L'
numlockx
export QT_STYLE_OVERRIDE=kvantum
export _JAVA_AWT_WM_NONREPARENTING=1
exec xmonad
