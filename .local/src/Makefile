TARGETS := ../bin/vpn

all: $(TARGETS)

clean:
	$(RM) $(TARGETS)

.PHONY: clean

../bin/%: %.c
	$(CC) -o $@ $<
	sudo chown root $@
	sudo chmod 4750 $@
