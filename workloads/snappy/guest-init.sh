#!/bin/bash

cd /root/
cd snappy
mkdir build
cd build
cmake ../
make
cd /root/
make

poweroff
