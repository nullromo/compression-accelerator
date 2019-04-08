package example

import chisel3.{Driver => _, _}
import chisel3.iotesters._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tile.OpcodeSet
import org.scalatest.{FlatSpec, Matchers}
import treadle.TreadleTester
import example.TreadleTesterMemFunctions._

class CompressionAcceleratorTester(c: ScratchpadTestModule) extends PeekPokeTester(c) {
  // set length
  poke(c.io.cmd.bits.inst.funct, 2) // doSetLength
  poke(c.io.cmd.bits.rs1, 100) // length = 100
  poke(c.io.cmd.valid, true) // fire
  step(1)
  // compress
  poke(c.io.cmd.bits.inst.funct, 0) // doCompress
  poke(c.io.cmd.bits.rs1, 0) // src = 0
  poke(c.io.cmd.bits.rs2, 100) // dst = 100
  step(1)
  poke(c.io.cmd.valid, false)
  step(100)
//  expect(peek(c.io.interrupt) != 277, "I should have passed ;(")

}

class CompressionAcceleratorSpec extends ChiselFlatSpec {
  implicit val p: Parameters = AcceleratorParams()

  val dutGen: () => ScratchpadTestModule = () => LazyModule(new ScratchpadTest(OpcodeSet.custom3)).module
  "CompressionAccelerator" should "accept commands" in {
    Driver.execute(TesterArgs() :+ "CompressionAccelerator", dutGen) {
      c => new CompressionAcceleratorTester(c)
    } should be(true)
  }
}

class TreadleTest extends FlatSpec with Matchers {
  implicit val p: Parameters = AcceleratorParams()

  "Something" should "do something" in {
    val s = chisel3.Driver.emit(() => LazyModule(new CompressionAccelerator(OpcodeSet.custom3)).module)
    implicit val tester: TreadleTester = new TreadleTester(s)

    tester.engine.makeVCDLogger("results/treadlevcd.vcd", showUnderscored = true)

    // set up memory
    val mem = "hashTable.mem_0"
    loadMemFromFile("memdata/memdata_0.txt", mem)

    // start compression
    tester.poke("io_cmd_bits_inst_funct", 2)
    tester.poke("io_cmd_bits_rs1", 100)
    tester.poke("io_cmd_valid", 1)
    tester.step()
    tester.poke("io_cmd_bits_inst_funct", 0)
    tester.poke("io_cmd_bits_rs1", 0)
    tester.poke("io_cmd_bits_rs2", 100)
    tester.step()
    tester.poke("io_cmd_valid", 0)

    //do testing
    var dump: Seq[BigInt] = Seq()

    for(i <- 0 until 500) {
      tester.step()
      val newDump = read128Mem(mem)
      if(dump != newDump) {
        dump = newDump
        println("Cycle " + i)
        dump128Mem(mem)
      }
    }

    tester.engine.writeVCD()
  }
}