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
  //      sEmitCopy: emit copy tags until there is no repeated data
  // sEmitRemainder: emit the end of a stream as a literal
  val sIdle :: sLookForMatch :: sEmitLiteral :: sEmitCopy :: sEmitRemainder :: Nil = Enum(5)
  val state = RegInit(sIdle)

  // get the RoCC command
  val cmd = Queue(io.cmd)

  // which operation is the command telling us to do?
  val doCompress: Bool = cmd.bits.inst.funct === 0.U
  val doUncompress: Bool = cmd.bits.inst.funct === 1.U
  val doSetLength: Bool = cmd.bits.inst.funct === 2.U

  // hold the length field given by the setLength command
  val length = RegInit(0.U(32.W))

  // hold the source and destination addresses for the compress and uncompress commands
  val src = RegInit(0.U(32.W))
  val dst = RegInit(0.U(32.W))

  // drives the busy signal to tell the CPU that the accelerator is busy
  val busy = RegInit(false.B)

  // tableSize-entry table, data is 16 bits for offset
  val hashTable = Module(BypassEnableMem(BypassEnableMemParameters(tableSize, 16, syncRead = false, bypass = false)))
  hashTable.io.writeEnable := false.B
  hashTable.io.writeAddress := 0.U
  hashTable.io.writeData := 0.U
  hashTable.io.readAddress := 0.U

  // components for state machine
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
  val bbhl      = RegInit( 0.U(32.W))
  val op        = RegInit( 0.U(32.W))
  val n         = RegInit( 0.U(32.W))
  val base      = RegInit( 0.U(32.W))
  val count     = RegInit( 0.U(32.W))
  dontTouch(ip_end)
  dontTouch(base_ip)
  dontTouch(next_emit)
  dontTouch(ip_limit)
  dontTouch(next_hash)
  dontTouch(skip)
  dontTouch(next_ip)
  dontTouch(candidate)
  dontTouch(bbhl)
  dontTouch(op)
  dontTouch(n)
  dontTouch(base)
  dontTouch(count)

  // state machine
  when(state === sLookForMatch) {
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
//    when(io.mem(next_ip) === io.mem(candidate)) {
      // pretend mem(7) === mem(0) TODO: [remove me]
      when(next_ip === 7.U) {
        state := sEmitLiteral
        count := 0.U
        base := op
        when(next_ip - next_emit > 60.U) {
          // length > 60 means we need to emit the the length partly as additional bytes after the tag, so we do that in the next state
          n := next_ip - next_emit - 1.U
        }.otherwise {
          // length <= 60 means we can encode the length in the tag byte, so do it now and set n to 0
          //TODO: figure out how to access the memory
          //        io.mem(op) = (n<<2).asUInt()
          n := 0.U
        }
        op := op + 1.U
      }
    }
  }.elsewhen(state === sEmitLiteral) {
    when(n > 0.U) {
      // n > 0 means we are emitting the tag byte(s) and n > 60
      //TODO: figure out how to access the memory
      //    io.mem(op) = n & 0xFF.U
      op := op + 1.U
      n := (n >> 8.U).asUInt()
      count := count + 1.U
      when((n >> 8.U).asUInt() <= 0.U) {
        // we have reached the end of the tag bytes
        //TODO: figure out how to access the memory
        //      io.mem(base) = ((count + 59.U) << 2.U).asUInt()
      }
    }.otherwise {
      // n <= 0 means we are just copying data
      //TODO: copy (next_emit - next_ip) bytes from next_emit to op and
      // set op = op + (next_emit - next_ip)
    }
  }.elsewhen(state === sEmitCopy) {

  }.elsewhen(state === sEmitRemainder) {

  }.otherwise /*(state === sIdle)*/ {
    when(cmd.fire()) {
      when(doSetLength) {
        length := cmd.bits.rs1
      }.elsewhen(doCompress) {
        busy := true.B
        src := cmd.bits.rs1
        dst := cmd.bits.rs2
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