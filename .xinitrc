#!/bin/bash

~/.config/xplugrc
xplugd

export GDK_SCALE=$(cat ~/.config/dpi-scale || echo 1)
export GDK_DPI_SCALE=$(bc <<<"scale=2; 1/$GDK_SCALE")
xrdb -merge <<<"Xft.dpi: $((96*GDK_SCALE))"
xrdb -merge ~/.config/Xresources

xset s off
xset dpms 0 0 0
xss-lock -- sh -c 'xset dpms 5; i3lock -nec 000000 && killall -q i3lock; xset dpms 0' &

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

numlockx

picom -b

eval $(dbus-launch --sh-syntax --exit-with-session)
dunst &

export _JAVA_AWT_WM_NONREPARENTING=1
exec xmonad
