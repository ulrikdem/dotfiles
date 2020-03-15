all: ../bin/nvidia-modules ../bin/vpn ../bin/wifi

../bin/%: %.c
	$(CC) -o $@ $<
	sudo chown root $@
	sudo chmod 4750 $@
