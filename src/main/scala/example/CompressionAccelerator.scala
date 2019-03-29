package example

import chisel3._
import chisel3.core.dontTouch
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile._

class CompressionAccelerator(opcodes: OpcodeSet)(implicit p: Parameters)
  extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new CompressionAcceleratorModule(this)
}

class CompressionAcceleratorModule(outer: CompressionAccelerator)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer) with HasCoreParameters {
  val tableSize = 16
  //TODO: change all the magic numbers to parameters

  // STATE MACHINE
  //          sIdle: waiting for a RoCC command
  //  sLookForMatch: searching for a 4-byte match
  //   sEmitLiteral: feed the literal opcode and data to the output
  // sEmitRemainder: emit the end of a stream as a literal
  val sIdle :: sLookForMatch :: sEmitLiteral :: sEmitRemainder :: Nil = Enum(4)
  val state = RegInit(sIdle)

  // get the RoCC command
  val cmd = Queue(io.cmd)

  // which operation is the command telling us to do?
  val doCompress: Bool = cmd.bits.inst.funct === 0.U
  val doUncompress: Bool = cmd.bits.inst.funct === 1.U
  val doSetLength: Bool = cmd.bits.inst.funct === 2.U

  // register to hold the length field given by the setLength command
  val length = RegInit(0.U(32.W))

  // registers to hold the source and destination addresses for the compress and uncompress commands
  val src = RegInit(0.U(32.W))
  val dst = RegInit(0.U(32.W))

  // register that drives the busy signal to tell the CPU that the accelerator is busy
  val busy = RegInit(false.B)

  // tableSize-entry table, data is 16 bits for offset and 32 bits for value
  val hashTable = Module(BypassEnableMem(BypassEnableMemParameters(tableSize, 48, syncRead = false, bypass = false)))
  hashTable.io.writeEnable := false.B
  hashTable.io.writeAddress := 0.U
  hashTable.io.writeData := 0.U
  hashTable.io.readAddress := 0.U

  // components for state machine
  val ip        = RegInit( 0.U(32.W))
  val shift: UInt = (32 - log2Floor(tableSize)).U
  val ip_end    = RegInit( 0.U(32.W))
  val base_ip   = RegInit( 0.U(32.W))
  val next_emit = RegInit( 0.U(32.W))
  val kInputMarginBytes: UInt = 15.U
  val ip_limit  = RegInit( 0.U(32.W))
  val next_hash = RegInit( 0.U(32.W))
  val skip      = RegInit(32.U(32.W))
  val next_ip   = RegInit( 0.U(32.W))
  val candidate = RegInit( 0.U(32.W))
  val hash      = RegInit( 0.U(32.W))
  val bbhl      = RegInit( 0.U(32.W))
  val op        = RegInit( 0.U(32.W))
  dontTouch(ip)
  dontTouch(ip_end)
  dontTouch(base_ip)
  dontTouch(next_emit)
  dontTouch(ip_limit)
  dontTouch(next_hash)
  dontTouch(skip)
  dontTouch(next_ip)
  dontTouch(candidate)
  dontTouch(hash)
  dontTouch(bbhl)
  dontTouch(op)

  // state machine
  when(state === sLookForMatch) {
    ip := next_ip
    hash := next_hash
    bbhl := (skip >> 5.U).asUInt()
    skip := skip + (skip >> 5.U).asUInt()
    next_ip := next_ip + (skip >> 5.U).asUInt()
    when(next_ip > ip_limit) {
      state := sEmitRemainder
    }.otherwise {
      next_hash := Hash(next_ip + (skip >> 5.U).asUInt(), shift)
      hashTable.io.readAddress := next_hash
      candidate := base_ip + hashTable.io.readData
      hashTable.io.writeEnable := true.B
      hashTable.io.writeAddress := next_hash
      hashTable.io.writeData := next_ip - base_ip

      //TODO: why do chisel printf's not support \t or %3d?
      printf("Compare:  mem[%d]  mem[%d]\n", candidate, next_ip)

      //TODO: figure out how to access the memory
//     when(io.mem(next_ip) === io.mem(candidate)) {
//       state := sEmitLiteral
//     }

      // pretend mem(7) === mem(0) TODO: [remove me]
      when(next_ip === 7.U) {
        state := sEmitLiteral
      }
    }
  }.elsewhen(state === sEmitLiteral) {
    
  }.elsewhen(state === sEmitRemainder) {

  }.otherwise /*(state === sIdle)*/ {
    when(cmd.fire()) {
      when(doSetLength) {
        length := cmd.bits.rs1
      }.elsewhen(doCompress) {
        busy := true.B
        src := cmd.bits.rs1
        dst := cmd.bits.rs2
        ip := cmd.bits.rs1 + 1.U
        ip_end := cmd.bits.rs1 + length
        base_ip := cmd.bits.rs1
        next_emit := cmd.bits.rs1
        when(length < kInputMarginBytes) {
          state := sEmitRemainder
        }.otherwise {
          state := sLookForMatch
          ip_limit := cmd.bits.rs1 + length - kInputMarginBytes
          next_hash := Hash(cmd.bits.rs1 + 1.U, shift)
          next_ip := cmd.bits.rs1 + 1.U
        }
      }.elsewhen(doUncompress) {
        busy := true.B
        // ...
      }
    }
  }




  io.mem.req.valid := false.B
  io.resp.valid := false.B
  io.resp.bits.rd := RegNext(io.resp.bits.rd)
  io.resp.bits.data := (-1).S(xLen.W).asUInt()

  cmd.ready := !busy
  io.busy := busy
  io.interrupt := false.B


}

object Hash {
  def apply(bytes: UInt, shift: UInt): UInt = {
    (bytes * 0x1e35a7bd.U >> shift).asUInt()
  }
}