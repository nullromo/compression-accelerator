package example

import chisel3.{Driver => _, _}
import chisel3.iotesters._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tile.OpcodeSet

class CompressionAcceleratorTester(c: CompressionAcceleratorModule) extends PeekPokeTester(c) {
  // set length
  poke(c.io.cmd.bits.inst.funct, 2) // doSetLength
  poke(c.io.cmd.bits.rs1, 100) // length = 100
  poke(c.io.cmd.valid, true) // fire
  step(1)
  // compress
  poke(c.io.cmd.bits.inst.funct, 0) // doCompress
  poke(c.io.cmd.bits.rs1, 0) // src = 0
  poke(c.io.cmd.bits.rs2, 100) // dst = 100
  step(100)
  expect(peek(c.io.interrupt) != 277, "I should have passed ;(")
}

class CompressionAcceleratorSpec extends ChiselFlatSpec {
  implicit val p: Parameters = AcceleratorParams()

  val dutGen: () => CompressionAcceleratorModule =
    () => LazyModule(new CompressionAccelerator(OpcodeSet.custom3)).module
  "CompressionAccelerator" should "accept commands" in {
    Driver.execute(TesterArgs() :+ "CompressionAccelerator", dutGen) {
      c => new CompressionAcceleratorTester(c)
    } should be(true)
  }
}

