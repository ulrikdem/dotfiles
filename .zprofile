if [[ $TTY = /dev/tty1 ]] && type startx; then
    exec startx
fi &>/dev/null
