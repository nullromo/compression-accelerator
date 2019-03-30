package example

import chisel3.{Driver => _, _}
import chisel3.iotesters._

/**
  * Tester for memory tester.
  */
class BasicMemoryTester(c: BasicMem) extends PeekPokeTester(c) {
  // create some dummy data
  val sampleData: Seq[Int] = Seq(123, 1, 5, 18, 99, 23, 7, 6)

  // put the data into the memory
  for ((d, a) <- sampleData.zipWithIndex)
    writeToMem(a, d)

  // read back all the data
  //  for ((_, a) <- sampleData.zipWithIndex)
  //    read(a)

  // read a write simultaneously
  poke(c.io.writeAddress, 0)
  poke(c.io.writeData, 700)
  poke(c.io.writeEnable, true)
  poke(c.io.readAddress, 1)
  println("Before step " + peek(c.io.readAddress) + ":" + peek(c.io.readData))
  step(1)
  println("After step " + peek(c.io.readAddress) + ":" + peek(c.io.readData))

  /**
    * Writes some data at some address.
    */
  def writeToMem(address: Int, data: Int): Unit = {
    poke(c.io.writeAddress, address)
    poke(c.io.writeData, data)
    poke(c.io.writeEnable, true)
    println("Writing " + address + ":" + data)
    step(1)
  }

  /**
    * Reads the data at some address.
    */
  def read(address: Int): Unit = {
    poke(c.io.readAddress, address)
    if (c.p.syncRead)
      step(1)
    val data = peek(c.io.readData)
    println("Reading " + address + ":" + data)
    expect(data == sampleData(address), "Data out was not data in.")
  }
}

class MultiPortMemoryTester(c: MultiPortMem) extends PeekPokeTester(c) {
  // write 3 out of 4 values
  poke(c.io.writeAddress, 0)
  poke(c.io.writeData(0), 45)
  poke(c.io.writeData(1), 78)
  poke(c.io.writeData(2), 99)
  poke(c.io.writeData(3), 2)
  poke(c.io.writeEnable(0), 1)
  poke(c.io.writeEnable(1), 0)
  poke(c.io.writeEnable(2), 1)
  poke(c.io.writeEnable(3), 1)
  step(1)

  // read back the values
  poke(c.io.readAddress, 0)
  val d1: BigInt = peek(c.io.readData(0))
  val d2: BigInt = peek(c.io.readData(1))
  val d3: BigInt = peek(c.io.readData(2))
  val d4: BigInt = peek(c.io.readData(3))
  println("got: " + d1 + " " + d2 + " " + d3 + " " + d4)
  expect(d1 == 45 && d2 == 0 && d3 == 99 && d4 == 2, "Writing and reading back doesn't work.")
  step(1)

  // write over with an offset
  poke(c.io.writeAddress, 2)
  poke(c.io.writeData(0), 7)
  poke(c.io.writeData(1), 18)
  poke(c.io.writeData(2), 100)
  poke(c.io.writeData(3), 444)
  poke(c.io.writeEnable(0), 1)
  poke(c.io.writeEnable(1), 0)
  poke(c.io.writeEnable(2), 1)
  poke(c.io.writeEnable(3), 1)
  step(1)

  // read back the values
  poke(c.io.readAddress, 2)
  val d5: BigInt = peek(c.io.readData(0))
  val d6: BigInt = peek(c.io.readData(1))
  val d7: BigInt = peek(c.io.readData(2))
  val d8: BigInt = peek(c.io.readData(3))
  println("got: " + d5 + " " + d6 + " " + d7 + " " + d8)
  expect(d5 == 7 && d6 == 2 && d7 == 100 && d8 == 444, "Overwriting doesn't work.")
  step(1)

  poke(c.io.readAddress, 0)
  val d9: BigInt = peek(c.io.readData(0))
  val d10: BigInt = peek(c.io.readData(1))
  println("got " + d9 + " " + d10)
  expect(d9 == 45 && d10 == 0, "Old values not retained.")
  step(3)
}

/**
  * Spec for memory tester.
  */
class MemoryTesterSpec extends ChiselFlatSpec {
  val dutGenBasic: () => BasicMem = () => BasicMem(MemParameters(256, 16, syncRead = false, bypass = false))
  "BasicMemoryTester" should "work properly" in {
    Driver.execute(TesterArgs() :+ "BasicMemoryTest", dutGenBasic) {
      c => new BasicMemoryTester(c)
    } should be(true)
  }

  val dutGenMultiPort: () => MultiPortMem = () => MultiPortMem(MemParameters(256, 16, syncRead = false, bypass = false), 4)
  "MultiPortMemoryTester" should "work properly" in {
    Driver.execute(TesterArgs() :+ "MultiPortMemoryTest", dutGenMultiPort) {
      c => new MultiPortMemoryTester(c)
    } should be(true)
  }
}