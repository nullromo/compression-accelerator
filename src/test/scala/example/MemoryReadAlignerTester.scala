package example

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import scala.io.Source

class MemoryReadAlignerTester(c: MemoryReadAlignerTestModule, sequential: Boolean) extends PeekPokeTester(c) {
  // load the memory data for comparison
  val memMap: Array[String] = Array.fill(800){""}
  var lineNum = 0
  for (line <- Source.fromFile("data/alignerTestData.txt").getLines()) {
    for(i <- 0 until 8) {
      val data1 = Integer.parseInt(line.substring(2*i, 2*i + 2), 16)
      memMap(8*lineNum + i) = format(data1, 1)
    }
    lineNum += 1
  }

  // how much to add to the address each time when doing non-sequential access
  val nonSequentialAddressPattern: Seq[Int] = Seq(4, -2, 1, 1, 1)

  // keep track of test cycles
  var cycles = 0

  // always sending valid addresses and always ready to receive
  poke(c.io.readIO.address.valid, true)
  poke(c.io.readIO.data.ready, true)

  // look up some addresses in order
  var numAddresses = 0
  var address = 0
  while (numAddresses < 30 && cycles < 1000) {

    // put in the address
    poke(c.io.readIO.address.bits, address)

    // read when the data is valid
    if (peek(c.io.readIO.data.valid) != 0) {
      val dataOut = peek(c.io.readIO.data.bits)
      val dataString = format(dataOut, 4)
      val expectedDataString = memMap(address) + memMap(address + 1) + memMap(address + 2) + memMap(address + 3)
      println("mem[" + f"$address%2d" + "] = " + dataString)
      println("was       " + expectedDataString)
      expect(dataString == expectedDataString, "wrong data :(")
    }

    // increment the address if ready to
    if (peek(c.io.readIO.address.ready) != 0) {
      if (sequential)
        address += 1
      else
        address += nonSequentialAddressPattern(numAddresses % nonSequentialAddressPattern.length)
      numAddresses += 1
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
    Driver.execute(TesterArgs() :+ "MemoryReadAlignerNonsequential", dutGen) {
      c => new MemoryReadAlignerTester(c, false)
    } should be(true)
  }
}