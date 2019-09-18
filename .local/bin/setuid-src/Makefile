SRC := $(wildcard *.c)

all: $(SRC:%.c=../%)

../%: %.c
	$(CC) -o $@ $<
	sudo chown root $@
	sudo chmod 4750 $@
