package example

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class MatchFinderTester(c: MatchFinder) extends PeekPokeTester(c) {

}

class MatchFinderSpec extends ChiselFlatSpec {
  val dutGen: () => MatchFinder = () => new MatchFinder(64, 32, 4096)
  "MatchFinder" should "find matches" in {
    Driver.execute(TesterArgs() :+ "MatchFinder", dutGen) {
      c => new MatchFinderTester(c)
    } should be(true)
  }
}