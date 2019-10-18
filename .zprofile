if [[ $(tty) = /dev/tty1 ]]; then
    exec startx
elif [[ $(tty) = /dev/tty2 && -f /usr/lib/modules/$(uname -r)/extramodules/nvidia.ko.gz ]]; then
    nvidia-modules load
    startx
    nvidia-modules unload
    exit
fi &>/dev/null
