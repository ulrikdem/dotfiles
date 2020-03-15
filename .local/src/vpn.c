#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv) {
    char uid[11];
    snprintf(uid, sizeof(uid), "#%u", getuid());

    char **args = malloc(sizeof(char*) * (8 + argc));
    args[0] = "/bin/ip";
    args[1] = "netns";
    args[2] = "exec";
    args[3] = "vpn";
    args[4] = "/usr/bin/sudo";
    args[5] = "-u";
    args[6] = uid;
    args[7] = "--";
    for (int i = 1; i < argc; ++i)
        args[7 + i] = argv[i];
    args[7 + argc] = NULL;

    setuid(0);
    return execv(args[0], args);
}
