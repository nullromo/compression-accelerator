#include "rocc.h"
#include <stdlib.h>
#include <stdio.h>

/*
 * The RoCC channel that this accelerator lives on
 */
#define CHANNEL 3

/*
 * Custom FUNCT fields for the accelerator
 */
#define COMPRESS 0      // Compress data
#define UNCOMPRESS 1    // Uncompress data
#define SET_LENGTH 2    // Tell the accelerator what the length field is. This instruction is necessary because the compression functions take 3 parameters and only 2 can be passed in via source registers in one instruction.

/*
 * Compresses LENGTH bytes from SOURCE into DEST and returns the compressed length.
 */
static inline unsigned long compress(char* source, int uncompressed_length, char* dest)
{
    unsigned long compressed_length;
    asm volatile ("fence"); //What does this do? Synchronize the caches maybe?
    ROCC_INSTRUCTION_S(CHANNEL, uncompressed_length, SET_LENGTH);
    ROCC_INSTRUCTION_DSS(CHANNEL, compressed_length, source, dest, COMPRESS);
    return compressed_length;
}

/*
 * Decompresses LENGTH bytes from SOURCE into DEST. Returns non-zero on failure.
 */
static inline unsigned long uncompress(char* source, int compressed_length, char* dest)
{
    unsigned long success;
    asm volatile ("fence"); //What does this do? Synchronize the caches maybe?
    ROCC_INSTRUCTION_S(CHANNEL, compressed_length, SET_LENGTH);
    ROCC_INSTRUCTION_DSS(CHANNEL, success, source, dest, UNCOMPRESS);
    return success;
}

int main(void)
{
    char original[256] = "If Tid the thistle sifter lifts his sieve of sifted thistles, then he'll shift the thistles in his sifting sieve to live unsifted.";
    int original_length = 130;
    char compressed[256]; //(char*) calloc(256, sizeof(char));
    char recovered[256]; //(char*) calloc(256, sizeof(char));

    printf("Compressing [%d bytes] \"%s\" ...\n", original_length, original);
    int compressed_length = compress(original, original_length, compressed);

    printf("... got [%d bytes] \"%s\" ...\n", compressed_length, compressed);

    printf("Uncompressing... \n");
    int success = uncompress(compressed, compressed_length, recovered);

    printf("Recovered \"%s\" result: %d.\n", recovered, success);

    return 0;
}
