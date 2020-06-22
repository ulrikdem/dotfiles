TARGETS := ../bin/igrep-format ../bin/nvidia-modules ../bin/vpn ../bin/wifi

all: $(TARGETS)

clean:
	$(RM) $(TARGETS)

.PHONY: clean

../bin/%: %.cpp
	$(CXX) -std=c++17 -O3 -o $@ $<

../bin/%: %.c
	$(CC) -o $@ $<
	sudo chown root $@
	sudo chmod 4750 $@
