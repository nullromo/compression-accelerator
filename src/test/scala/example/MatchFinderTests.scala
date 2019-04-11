package example

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import scala.util.control.Breaks._

class MatchFinderTester(c: MatchFinderTestModule) extends PeekPokeTester(c) {
  // keep track of test cycles
  var cycles = 0

  // always be ready for output
  poke(c.io.out.ready, true)

  // set global base
  poke(c.io.globalBase, 100)

  // keep track of pointers
  var base: BigInt = 100
  var end: BigInt = 100

  // count matches found
  var matches = 0

  // find matches
  while(base < 1024) {

    // send in some input
    poke(c.io.start.valid, true)
    poke(c.io.start.bits, base)
    s(1)

    // stop sending input
    poke(c.io.start.valid, false)

    // wait for output to be valid (but don't wait forever)
    while (peek(c.io.out.valid) == 0) {
      if(cycles > 10000) {
        println("Too many cycles")
        break
      }
      s(1)
    }

    // match found!
    matches += 1
    base = peek(c.io.out.bits.matchA)
    end = peek(c.io.out.bits.matchB)
    println("match found: " + base + " to " + end)

    // set up for the next search
    base = end + 1
    s(1)
  }

  println(matches + " matches found.")

  def s(n: Int): Unit = {
    step(n)
    cycles += n
  }
}

class MatchFinderSpec extends ChiselFlatSpec {
  val dutGen: () => MatchFinderTestModule = () => new MatchFinderTestModule(8, 32, 512)
  "MatchFinder" should "find matches" in {
    Driver.execute(TesterArgs() :+ "MatchFinder", dutGen) {
      c => new MatchFinderTester(c)
    } should be(true)
  }
}