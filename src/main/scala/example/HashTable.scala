package example

import chisel3.util.log2Floor
import chisel3._

/**
  * Hash Table for compression. Takes in the new data at a given location and the offset of the new data from the base,
  * and gives the old data and offset at that same hash.
  */
class HashTable(dataWidth: Int, offsetWidth: Int, hashTableSize: Int) extends Module {
  val io = IO(new Bundle {
    val newData = Input(UInt(dataWidth.W))
    val newOffset = Input(UInt(offsetWidth.W))
    val enable = Input(Bool())
    val oldData = Output(UInt(dataWidth.W))
    val oldOffset = Output(UInt(offsetWidth.W))
  })

  // hash the new data to get the table address
  val address: UInt = hash(io.newData, (32 - log2Floor(hashTableSize)).U)

  // create the underlying memories that hold the 2 columns of the table
  val offsetMem = Mem(hashTableSize, UInt(offsetWidth.W))
  val dataMem = Mem(hashTableSize, UInt(dataWidth.W))

  // write to the table
  when(io.enable) {
    offsetMem.write(address, io.newOffset)
    dataMem.write(address, io.newData)
  }

  // read from the table
  io.oldOffset := offsetMem.read(address)
  io.oldData := dataMem.read(address)

  // hash function
  def hash(bytes: UInt, shift: UInt): UInt = {
    (bytes * 0x1e35a7bd.U >> shift).asUInt()
  }
}