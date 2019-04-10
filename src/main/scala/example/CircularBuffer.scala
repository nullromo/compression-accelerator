package example

import chisel3.util.log2Ceil
import chisel3._

/**
  * Controller for circular buffer. Cannot read and write simultaneously when empty or full.
  * Use read and write signals to add and consume data. The underlying data structure is separate,
  * so it can be read or written without this module's knowledge.
  *
  * @param length number of slots in buffer
  * @param width  number of bits in each slot
  */
class CircularBuffer(length: Int, width: Int) extends Module {
  val io = IO(new Bundle {
    val head = Output(UInt(width.W))
    val tail = Output(UInt(width.W))
    val empty = Output(Bool())
    val full = Output(Bool())
    val read = Input(Bool())
    val write = Input(Bool())
  })

  val head = RegInit(0.U(width.W))
  val tail = RegInit(1.U(width.W))
  val empty = RegInit(true.B)
  val size = RegInit(0.U(log2Ceil(length).W))

  when(io.write) {
    when(head === tail) {printf("Error: head was the same as tail!")}
    empty := false.B
    when(!empty) {
      tail := tail + 1.U
    }
  }

  when(io.read) {
    when(empty) {printf("Error: cannot read from empty buffer!")}
    when(head === tail - 1.U) {
      empty := true.B
    }.otherwise {
      head := head + 1.U
    }
  }

  io.head := head
  io.tail := tail
  io.empty := empty
  io.full := head === tail
}
