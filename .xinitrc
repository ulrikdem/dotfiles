#!/bin/bash

dpi=$(cat ~/.config/dpi || echo 96)
export GDK_SCALE=$((dpi / 96))
export GDK_DPI_SCALE=$(bc <<<"scale=2; 1/$GDK_SCALE")
xrdb -merge <<<"Xft.dpi: $dpi"
xrdb -merge ~/.config/Xresources

dbus-update-activation-environment --systemd DISPLAY XAUTHORITY

~/.config/xplugrc
xplugd

xset s off
xset dpms 0 0 0
xss-lock -- sh -c 'xset dpms 5; i3lock -nec 000000 && killall -q i3lock; xset dpms 0' &

numlockx

picom &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

export _JAVA_AWT_WM_NONREPARENTING=1
exec xmonad
