TARGETS := ../bin/igrep-format ../bin/nvidia-modules ../bin/vpn

all: $(TARGETS)

clean:
	$(RM) $(TARGETS)

.PHONY: clean

../bin/%: %.cpp
	$(CXX) -std=c++20 -O3 -o $@ $<

../bin/%: %.c
	$(CC) -o $@ $<
	sudo chown root $@
	sudo chmod 4750 $@
