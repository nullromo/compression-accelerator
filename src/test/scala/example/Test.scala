package example

import chisel3.{Bundle, Input, Module, Output, Bool}
import chisel3.iotesters._

class ASDF extends Module {
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
  val testerArgs = Array(
    "-fiwv",
    "--backend-name", "treadle",
    "--tr-write-vcd",
    "--target-dir", "test_run_dir/creec",
    "--top-name")

  "ASDF" should "do something" in {
    Driver.execute(testerArgs :+ "ASDF", () => new ASDF) {
      c =>
      new ASDFTester(c)
    } should be(true)
  }
}