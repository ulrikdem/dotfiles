all: ../bin/igrep-format ../bin/nvidia-modules ../bin/vpn ../bin/wifi

../bin/%: %.cpp
	$(CXX) -std=c++17 -O3 -o $@ $<

../bin/%: %.c
	$(CC) -o $@ $<
	sudo chown root $@
	sudo chmod 4750 $@
