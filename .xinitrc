#!/bin/bash
xrandr --auto
if xrandr --listproviders | grep -q NVIDIA; then
    xprop -root -f GPU_ICON 8u -set GPU_ICON ' '
else
    xprop -root -f GPU_ICON 8u -set GPU_ICON ''
fi
feh --no-fehbg --bg-fill ~/.config/wallpaper.jpg
picom -b
xmodmap -e 'remove Lock = Caps_Lock' -e 'keysym Caps_Lock = Control_L' -e 'add Control = Control_L'
numlockx
export QT_STYLE_OVERRIDE=kvantum
export _JAVA_AWT_WM_NONREPARENTING=1
exec xmonad
