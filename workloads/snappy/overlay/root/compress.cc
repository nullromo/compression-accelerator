#include <stdio.h>
#include <string.h>
#include "snappy/snappy.h"

static inline unsigned long read_cycles(void)
{
    unsigned long cycles;
    //asm volatile ("rdcycle %0" : "=r" (cycles));
    return cycles;
}

int run(std::string input, int *compressed_length)
{
    std::string output;
    unsigned long start, end;
    start = read_cycles();
    *compressed_length = snappy::Compress(input.data(), input.size(), &output);
    end = read_cycles();
    return end - start;
}

int main(int argc, char* argv[])
{
    const std::string input = argv[1];
    printf("input is %s\n", input.c_str());
    int runs = 20;
    int total = 0;
    int compressed_length;
    for(int i=0; i<runs; i++)
    {
        total += run(input, &compressed_length);
    }
    int average = total / runs;
    printf(": %lu : %d\n", average, compressed_length);
}
