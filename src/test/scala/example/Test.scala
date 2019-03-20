package example

import chisel3.iotesters._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tile.OpcodeSet

class CompressionAcceleratorTester(c: CompressionAcceleratorModule) extends PeekPokeTester(c) {
  //TODO: this is nonsense
  poke(c.io.cmd.bits.rs2, 0)
  step(2)
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