package example

import example.TreadleTesterMemFunctions._
import org.scalatest.{FlatSpec, Matchers}
import treadle.TreadleTester

class DecoupledArbiterTest extends FlatSpec with Matchers {
  "DecoupledArbiter" should "function" in {
    val s = chisel3.Driver.emit(() => new ArbiterWithMem(32, 32, 4))
    implicit val tester: TreadleTester = new TreadleTester(s)

    tester.engine.makeVCDLogger("results/treadle_arbiter.vcd", showUnderscored = true)

    tester.step(8)

    // set up memory
    val mem = "mem.mem_0"
    loadMemFromFile("memdata/memdata_0.txt", mem)

    // always be ready to accept data
    tester.poke("io_data_ready", 1)

    // feed in valid address
    tester.poke("io_address_bits_0", 1)
    tester.poke("io_address_bits_1", 2)
    tester.poke("io_address_bits_2", 3)
    tester.poke("io_address_bits_3", 4)
    tester.poke("io_address_valid", 1)
    tester.step(1)

    tester.poke("io_address_valid", 0)

    while (tester.peek("io_data_valid") == 0) {
      tester.step(1)
    }

    tester.expect("io_data_bits_0", 5)
    tester.expect("io_data_bits_1", 6)
    tester.expect("io_data_bits_2", 4)
    tester.expect("io_data_bits_3", 3)

    tester.step(10)

    tester.engine.writeVCD()
  }
}