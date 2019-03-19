package example

import chisel3.{Bool, Bundle, Input, Module, Output}
import chisel3.iotesters._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.OpcodeSet

object TesterArgs {
  def apply() = {
    Array(
      "-fiwv",
      "--backend-name", "treadle",
      "--tr-write-vcd",
      "--target-dir", "test_run_dir/creec",
      "--top-name")
  }
}

class ASDF(something: Int) extends Module {
  val io = IO(new Bundle{
    val in = Input(Bool())
    val out = Output(Bool())
  })
  io.out := !io.in
}

class ASDFTester(c: ASDF) extends PeekPokeTester(c) {
  poke(c.io.in, false)
  step(2)
  expect(peek(c.io.out) != 0, "help me")
}

class Tests extends ChiselFlatSpec {
  "ASDF" should "do something" in {
    Driver.execute(TesterArgs() :+ "ASDF", () => new ASDF(5)) {
      c =>
      new ASDFTester(c)
    } should be(true)
  }
}

class CompressionAcceleratorTester(c: CompressionAcceleratorModule) extends PeekPokeTester(c) {

}

class CompressionAcceleratorSpec extends ChiselFlatSpec {
  implicit val p: Parameters.type = Parameters
  "CompressionAccelerator" should "accept commands" in {
    Driver.execute(TesterArgs() :+ "CompressionAccelerator", () => new CompressionAcceleratorModule(new CompressionAccelerator(OpcodeSet.custom3))) {
      c => new CompressionAcceleratorTester(c)
    } should be (true)
  }
}