package example

import chisel3.{Driver => _, _}
import chisel3.iotesters._
import chisel3.util.log2Ceil

/**
  * Tester for memory tester.
  */
class MemoryTester(c: MemoryTestModule) extends PeekPokeTester(c) {
  // create some dummy data
  val writeData: Seq[Int] = Seq(123, 1, 5, 18, 99, 23, 7, 6)

  // put the data into the memory
  for ((d, a) <- writeData.zipWithIndex)
    write(a, d)

  // read back all the data
  for ((_, a) <- writeData.zipWithIndex)
    read(a)

  /**
    * Writes some data at some address.
    */
  def write(address: Int, data: Int): Unit = {
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
    expect(data == writeData(address), "Data out was not data in.")
  }
}

/**
  * Spec for memory tester.
  */
class MemoryTesterSpec extends ChiselFlatSpec {
  val dutGen: () => MemoryTestModule = () => MemoryTestModule(MemoryTestParameters(256, 48, syncRead = false))
  "MemoryTest" should "work properly" in {
    Driver.execute(TesterArgs() :+ "MemoryTest", dutGen) {
      c => new MemoryTester(c)
    } should be(true)
  }
}

/**
  * Parameters for basic Mem module.
  *
  * @param numEntries number of entries in the memory.
  * @param dataWidth  number of data bits.
  * @param syncRead   whether or not the data comes out when the read address changes or one cycle later.
  */
case class MemoryTestParameters(numEntries: Int, dataWidth: Int, syncRead: Boolean) {
  val addressWidth = log2Ceil(numEntries)
}

/**
  * Basic module that just instantiates a Mem.
  */
case class MemoryTestModule(p: MemoryTestParameters) extends Module {
  val io = IO(new Bundle {
    val readAddress = Input(UInt(p.addressWidth.W))
    val readData = Output(UInt(p.dataWidth.W))
    val writeAddress = Input(UInt(p.addressWidth.W))
    val writeData = Input(Vec(1, UInt(p.dataWidth.W)))
    val writeEnable = Input(Vec(1, Bool()))
  })

  val mem: MemBase[Vec[UInt]] =
    if (p.syncRead) {
      SyncReadMem(p.numEntries, Vec(1, UInt(p.dataWidth.W)))
    } else {
      Mem(p.numEntries, Vec(1, UInt(p.dataWidth.W)))
    }
  mem.write(io.writeAddress, io.writeData, io.writeEnable)
  io.readData := mem.read(io.readAddress)(0)
}