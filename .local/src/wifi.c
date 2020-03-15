#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
    char *args[4] = {NULL, NULL, NULL, NULL};
    if (argc == 1) {
        args[0] = "/bin/sh";
        args[1] = "-c";
        args[2] = "/usr/bin/wpa_cli scan_results | /usr/bin/tail -n +3 | /usr/bin/awk '{print $3,$5,\"\t\",$4}' | /usr/bin/sort -nr";
    } else if (!strcmp(argv[1], "start") || !strcmp(argv[1], "stop") || !strcmp(argv[1], "restart") || !strcmp(argv[1], "status")) {
        args[0] = "/bin/systemctl";
        args[1] = argv[1];
        args[2] = "netctl-auto@wlan0.service";
    } else if (!strcmp(argv[1], "menu")) {
        args[0] = "/usr/bin/wifi-menu";
    } else {
        args[0] = "/usr/bin/netctl-auto";
        args[1] = "switch-to";
        size_t n = strlen(argv[1]) + 7;
        args[2] = malloc(n);
        snprintf(args[2], n, "wlan0-%s", argv[1]);
    }
    setuid(0);
    setgid(0);
    return execv(args[0], args);
}
