package example

import chisel3._
import chisel3.util.log2Ceil

/**
  * Parameters for basic memory module.
  *
  * @param numEntries number of entries in the memory.
  * @param dataWidth  number of data bits.
  * @param syncRead   whether the data comes out when the read address changes or one cycle later.
  */
case class BypassEnableMemParameters(numEntries: Int, dataWidth: Int, syncRead: Boolean, bypass: Boolean) {
  val addressWidth = log2Ceil(numEntries)
}

/**
  * Mem with a write enable signal and a bypass that allows the read port to get the data being written before it is written.
  */
case class BypassEnableMem(p: BypassEnableMemParameters) extends Module {
  val io = IO(new Bundle {
    val readAddress = Input(UInt(p.addressWidth.W))
    val readData = Output(UInt(p.dataWidth.W))
    val writeAddress = Input(UInt(p.addressWidth.W))
    val writeData = Input(UInt(p.dataWidth.W))
    val writeEnable = Input(Bool())
  })

  val mem: MemBase[Vec[UInt]] =
    if (p.syncRead) {
      SyncReadMem(p.numEntries, Vec(1, UInt(p.dataWidth.W)))
    } else {
      Mem(p.numEntries, Vec(1, UInt(p.dataWidth.W)))
    }
  val wrEnable: Vec[Bool] = Wire(Vec(1, Bool()))
  val wrData: Vec[UInt] = Wire(Vec(1, UInt(p.dataWidth.W)))
  wrEnable(0) := io.writeEnable
  wrData(0) := io.writeData
  mem.write(io.writeAddress, wrData, wrEnable)
  if(p.bypass)
    io.readData := Mux(io.writeAddress === io.readAddress, io.writeData, mem.read(io.readAddress)(0))
  else
    io.readData := mem.read(io.readAddress)(0)
}