#include <stdio.h>
#include <string.h>
#include <iostream>
#include "snappy/snappy.h"

//const char* original_data = "The quick brown fox jumps over the lazy dog. If Tid the thistle sifter lifts his sieve of sifted thistles, he'll shift the thistles in his sifting sieve to live unsifted.";
//const char* original_data = "This is an example string. It is filled with all sorts of examply data and stuff. Look at all the byes. Wow. There are so many. This is the best data I have ever seen, and I am so happy that I am alive on this earth. Computers are great. When a computer processes data, it is a very great thing. There is so much data processing that needs to happen and it is all done by a computer. Wow.....";
const char* original_data = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
std::string compressed_data;
std::string recovered_data;

void print_compressed_data(std::string data, size_t length)
{
    for(int i=0; i<length; i++)
        if(data[i] == 0xd)
            std::cout << "\n";
        else
            std::cout << data[i];
    std::cout << "\n\n";
}

int main(int argc, char* argv[])
{
    std::cout << "Original (" << strlen(original_data) << " bytes):\n";
    std::cout << original_data << "\n\n";

    size_t compressed_length = snappy::Compress(original_data, strlen(original_data), &compressed_data);
    std::cout << "Compressed (" << compressed_length << " bytes):\n";
    print_compressed_data(compressed_data, compressed_length);

    if(!snappy::Uncompress(compressed_data.c_str(), compressed_length, &recovered_data))
        printf("Failed to recover data.\n\n");
    else
    {
        std::cout << "Recovered: (" << strlen(recovered_data.c_str()) << " bytes)\n";
        std::cout << recovered_data << "\n\n";
    }
    return 0;
}
