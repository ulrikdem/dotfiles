#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
    setuid(0);
    if (argc == 2 && !strcmp(argv[1], "load")) {
        return system("/sbin/modprobe -i nvidia && /sbin/modprobe nvidia-drm");
    } else if (argc == 2 && !strcmp(argv[1], "unload")) {
        return system("/sbin/modprobe -r nvidia-drm nvidia");
    } else {
        fputs("nvidia-modules load|unload\n", stderr);
        return 1;
    }
    return 0;
}
