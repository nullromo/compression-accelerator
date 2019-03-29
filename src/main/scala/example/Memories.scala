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
                         bypass: Boolean,
                         entriesPerWord: Int) {
  val addressWidth: Int = log2Ceil(numEntries)
  val numWords: Int = numEntries / entriesPerWord
  val bitsPerWord: Int = bitsPerEntry * entriesPerWord
  require(numEntries % entriesPerWord == 0)
}

/**
  * Memory that is structured using words.
  */
case class MaskedMem(p: MemParameters) extends Module {
  val io = IO(new Bundle {
    val readAddress = Input(UInt(p.addressWidth.W))
    val readData = Output(UInt(p.bitsPerWord.W))
    val writeAddress = Input(UInt(p.addressWidth.W))
    val writeData = Input(UInt(p.bitsPerWord.W))
    val writeEnable = Input(Vec(p.entriesPerWord, Bool()))
  })

  val mem = Mem(p.numWords, Vec(p.entriesPerWord, UInt(p.bitsPerEntry.W)))
  val rawData: Vec[UInt] = Wire(Vec(p.entriesPerWord, UInt(p.bitsPerEntry.W)))
  val rawDataNext: Vec[UInt] = Wire(Vec(p.entriesPerWord, UInt(p.bitsPerEntry.W)))
  rawData := mem.read(io.readAddress / p.entriesPerWord.U)
  rawDataNext := mem.read(io.readAddress / p.entriesPerWord.U + 1.U)
  for(i <- 0 to p.entriesPerWord) {
    io.readData := Cat(io.readData, VectorCatGrab(io.readAddress % p.entriesPerWord.U + i.U, rawData, rawDataNext)(p.entriesPerWord, p.bitsPerEntry))
  }
  

  def VectorCatGrab(index: UInt, a: Vec[UInt], b: Vec[UInt])(length: Int, elementSize: Int): UInt = {
    //  val io = IO(new Bundle {
    //    val index = Input(UInt((log2Ceil(length) + 1).W))
    //    val a = Input(Vec(length, UInt(elementSize.W)))
    //    val b = Input(Vec(length, UInt(elementSize.W)))
    //    val out = Output(UInt(elementSize.W))
    //  })
    val out = Wire(UInt(elementSize.W))
    when(index < length.U) {
      out := a(index)
    }.otherwise {
      out := b(index - length.U)
    }
    out
  }
}


/**
  * Mem with a write enable signal and a bypass that allows the read port to get the data being written before it is written.
  */
case class BasicMem(p: MemParameters) extends Module {
  val io = IO(new Bundle {
    val readAddress = Input(UInt(p.addressWidth.W))
    val readData = Output(UInt(p.bitsPerWord.W))
    val writeAddress = Input(UInt(p.addressWidth.W))
    val writeData = Input(UInt(p.bitsPerWord.W))
    val writeEnable = Input(Bool())
  })

  val mem: MemBase[Vec[UInt]] =
    if (p.syncRead) {
      SyncReadMem(p.numEntries, Vec(1, UInt(p.bitsPerWord.W)))
    } else {
      Mem(p.numEntries, Vec(1, UInt(p.bitsPerWord.W)))
    }
  val wrEnable: Vec[Bool] = Wire(Vec(1, Bool()))
  val wrData: Vec[UInt] = Wire(Vec(1, UInt(p.bitsPerWord.W)))
  wrEnable(0) := io.writeEnable
  wrData(0) := io.writeData
  mem.write(io.writeAddress, wrData, wrEnable)
  if (p.bypass)
    io.readData := Mux(io.writeAddress === io.readAddress, io.writeData, mem.read(io.readAddress)(0))
  else
    io.readData := mem.read(io.readAddress)(0)
}