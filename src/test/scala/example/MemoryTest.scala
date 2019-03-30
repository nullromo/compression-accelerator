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
  poke(c.io.writeData(0), 700)
  poke(c.io.writeEnable(0), true)
  poke(c.io.readAddress, 1)
  println("Before step " + peek(c.io.readAddress) + ":" + peek(c.io.readData))
  step(1)
  println("After step " + peek(c.io.readAddress) + ":" + peek(c.io.readData))

  /**
    * Writes some data at some address.
    */
  def writeToMem(address: Int, data: Int): Unit = {
    poke(c.io.writeAddress, address)
    poke(c.io.writeData(0), data)
    poke(c.io.writeEnable(0), true)
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
  poke(c.io.writeAddress, 0)
  poke(c.io.writeData(0), 45)
  poke(c.io.writeData(1), 78)
  poke(c.io.writeData(2), 99)
  poke(c.io.writeData(3), 2)
  poke(c.io.writeEnable(0), 1)
  poke(c.io.writeEnable(0), 0)
  poke(c.io.writeEnable(0), 1)
  poke(c.io.writeEnable(0), 1)
  step(1)

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

//  val dutGenMultiPort: () => MultiPortMem = () => MultiPortMem(MemParameters(256, 16, syncRead = false, bypass = false), 4)
//  "MultiPortMemoryTester" should "work properly" in {
//    Driver.execute(TesterArgs() :+ "MultiPortMemoryTest", dutGenMultiPort) {
//      c => new MultiPortMemoryTester(c)
//    } should be(true)
//  }
}