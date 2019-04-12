package example

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class MemoryReadAlignerTester(c: MemoryReadAlignerTestModule, sequential: Boolean) extends PeekPokeTester(c) {
  // keep track of test cycles
  var cycles = 0

  // always sending valid addresses and always ready to receive
  poke(c.io.readIO.address.valid, true)
  poke(c.io.readIO.data.ready, true)

  // look up some addresses in order
  var numAddresses = 0
  var address = 0
  while (numAddresses < 30) {

    // put in the address
    poke(c.io.readIO.address.bits, address)

    // read when the data is valid
    if (peek(c.io.readIO.data.valid) != 0 && cycles < 1000) {
      val dataOut = peek(c.io.readIO.data.bits)
      println("mem[" + address + "] = " + format(dataOut, 4))
    }

    // increment the address if ready to
    if (peek(c.io.readIO.address.ready) != 0) {
      numAddresses += 1
      if (sequential)
        address += 1
      else
        address += 3
    }

    s(1)

    expect(cycles < 1000, "Too many cycles")
  }

  def s(n: Int): Unit = {
    step(n)
    cycles += n
  }

  def format(data: BigInt, bytes: Int): String = {
    var result = ""
    for (shift <- 0 until bytes) {
      result = f"${(data >> (shift * 8)) & 0xFF}%X" + " " + result
    }
    result
  }
}

class MemoryReadAlignerSpec extends ChiselFlatSpec {
  val dutGen: () => MemoryReadAlignerTestModule = () => new MemoryReadAlignerTestModule(12, 32, 8, 64)
  "MemoryReadAligner" should "convert addresses properly" in {
    Driver.execute(TesterArgs() :+ "MemoryReadAligner", dutGen) {
      c => new MemoryReadAlignerTester(c, true)
    } should be(true)
  }

  "MemoryReadAligner" should "not mess up when the reads aren't sequential" in {
    Driver.execute(TesterArgs() :+ "MemoryReadAligner", dutGen) {
      c => new MemoryReadAlignerTester(c, false)
    } should be(true)
  }
}