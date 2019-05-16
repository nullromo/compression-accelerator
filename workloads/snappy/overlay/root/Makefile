CXX=g++

#CXXFLAGS=-levent -lzmq -lrt --sysroot=../../../wlutil/br/buildroot/output/host/usr/riscv64-buildroot-linux-gnu/sysroot -pthread -O3 -Wall -g -std=c++11 -D_GNU_SOURCE
CXXFLAGS=-std=c++11 -O3
IFLAGS=-Isnappy/build/

all:
	$(CXX) $(CXXFLAGS) $(IFLAGS) compress.cc snappy/build/libsnappy.a -o compress

