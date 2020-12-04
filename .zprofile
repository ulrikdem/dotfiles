if type -p startx; then
    if [[ $(tty) = /dev/tty1 ]]; then
        exec startx
    elif [[ $(tty) = /dev/tty2 && -f /usr/lib/modules/$(uname -r)/extramodules/nvidia.ko.xz ]] && type -p nvidia-modules; then
        nvidia-modules load
        startx
        nvidia-modules unload
        exit
    fi
fi &>/dev/null
