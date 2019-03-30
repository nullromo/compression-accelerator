package example

import chisel3._
import chisel3.util.{Cat, log2Ceil}

/**
  * Parameters for memory modules.
  *
  * @param syncRead     whether the data comes out when the read address changes or one cycle later.
  * @param bypass       whether or not the read data can be read before it is written.
  * @param numEntries   number of entries in the memory.
  * @param bitsPerEntry number of data bits.
  */
case class MemParameters(numEntries: Int,
                         bitsPerEntry: Int,
                         syncRead: Boolean,
                         bypass: Boolean) {
  val addressWidth: Int = log2Ceil(numEntries)
}

/**
  * Memory with multiple read and write ports.
  */
case class MultiPortMem(p: MemParameters, numPorts: Int) extends Module {
  val io = IO(new Bundle {
    val readAddress = Input(UInt(p.addressWidth.W))
    val readData = Output(Vec(numPorts, UInt(p.bitsPerEntry.W)))
    val writeAddress = Input(UInt(p.addressWidth.W))
    val writeData = Input(Vec(numPorts, UInt(p.bitsPerEntry.W)))
    val writeEnable = Input(Vec(numPorts, Bool()))
  })

  val mem = Mem(p.numEntries, Vec(1, UInt(p.bitsPerEntry.W)))
  for(i <- 0 until numPorts) {
    val wrEnable: Vec[Bool] = Wire(Vec(1, Bool()))
    val wrData: Vec[UInt] = Wire(Vec(1, UInt(p.bitsPerEntry.W)))
    wrEnable(0) := io.writeEnable(i)
    wrData(0) := io.writeData(i)
    mem.write(io.writeAddress + i.U, wrData, wrEnable)

    io.readData(i) := mem.read(io.readAddress + i.U)(0)
  }
}


/**
  * Mem with a write enable signal and a bypass that allows the read port to get the data being written before it is written.
  */
case class BasicMem(p: MemParameters) extends Module {
  val io = IO(new Bundle {
    val readAddress = Input(UInt(p.addressWidth.W))
    val readData = Output(UInt(p.bitsPerEntry.W))
    val writeAddress = Input(UInt(p.addressWidth.W))
    val writeData = Input(UInt(p.bitsPerEntry.W))
    val writeEnable = Input(Bool())
  })

  val mem: MemBase[Vec[UInt]] =
    if (p.syncRead) {
      SyncReadMem(p.numEntries, Vec(1, UInt(p.bitsPerEntry.W)))
    } else {
      Mem(p.numEntries, Vec(1, UInt(p.bitsPerEntry.W)))
    }
  val wrEnable: Vec[Bool] = Wire(Vec(1, Bool()))
  val wrData: Vec[UInt] = Wire(Vec(1, UInt(p.bitsPerEntry.W)))
  wrEnable(0) := io.writeEnable
  wrData(0) := io.writeData
  mem.write(io.writeAddress, wrData, wrEnable)
  if (p.bypass)
    io.readData := Mux(io.writeAddress === io.readAddress, io.writeData, mem.read(io.readAddress)(0))
  else
    io.readData := mem.read(io.readAddress)(0)
}