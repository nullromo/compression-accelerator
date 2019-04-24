package example

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class MatchFinderTester(c: MatchFinderTestModule) extends PeekPokeTester(c) {
    // keep track of test cycles
    var cycles = 0

    // hold all the matches found by the module
    var matchesFound: Array[(BigInt, BigInt)] = Array[(BigInt, BigInt)]()

    // always be ready for output and always send valid input
    poke(c.io.matchA.ready, true)
    poke(c.io.start.valid, true)

    // set global base
    poke(c.io.src, 0)

    // keep track of pointers
    var base: BigInt = 0
    var end: BigInt = 0
    var matchB = 0

    // count matches found
    var matches = 0

    // find matches
    while (base < 69 && cycles < 1000) {
        poke(c.io.matchB, matchB)
        // send in the input
        poke(c.io.start.bits, base)

        // if the output is valid, deal with it
        if (peek(c.io.matchA.valid) != 0) {
            base = peek(c.io.matchA.bits)
            end = matchB
            matches += 1
            println("match found: " + base + " to " + end)
            matchesFound = matchesFound :+ (base, end)
            base = end + 1
        }

        s(1)
    }

    println(matches + " matches found.")

    // expected matches based on file
    val expectedMatches = Array(
        (0, 44), (20, 57), (16, 70)
    )

    expect(expectedMatches sameElements matchesFound, "Wrong matches :(")

    def s(n: Int): Unit = {
        step(n)
        cycles += n
        matchB += 1
    }
}

class MatchFinderSpec extends ChiselFlatSpec {
    val dutGen: () => MatchFinderTestModule = () => new MatchFinderTestModule(
        64, 32, 32, 512)
    "MatchFinder" should "find matches" in {
        Driver.execute(TesterArgs() :+ "MatchFinder", dutGen) {
            c => new MatchFinderTester(c)
        } should be(true)
    }
}