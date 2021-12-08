if type startx; then
    if [[ $TTY = /dev/tty1 ]]; then
        exec startx
    elif [[ $TTY = /dev/tty2 && -f /usr/lib/modules/$(uname -r)/extramodules/nvidia.ko.xz ]] && type nvidia-modules; then
        nvidia-modules load
        startx
        nvidia-modules unload
        exit
    fi
fi &>/dev/null
