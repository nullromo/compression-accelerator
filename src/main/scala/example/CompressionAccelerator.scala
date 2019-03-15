package example

import Chisel.{Bits, Bool, Cat, Enum, Mux, Reg, SInt, UInt, when}
import chisel3._
import freechips.rocketchip.config.{Field, Parameters, View}
import freechips.rocketchip.rocket.PTE
import freechips.rocketchip.tile._

class CompressionAccelerator(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new CompressionAcceleratorModule(this)
}

class CompressionAcceleratorModule(outer: CompressionAccelerator)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) with HasCoreParameters {

  /*
  val sIdle :: sLookForMatch :: sEmitLiteral :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val cmd = Queue(io.cmd)
  val doCompress: Bool = cmd.bits.inst.funct === UInt(0)
  val doUncompress: Bool = cmd.bits.inst.funct === UInt(1)
  val doSetLength: Bool = cmd.bits.inst.funct === UInt(2)

  val length = RegInit(0.U(32.W))
  val busy = RegInit(false.B)

  //256-entry table, data is 16 bits for offset and 32 bits for value
  val hashTable = Mem(256, UInt(48.W))

  //when cmd fires, kick off the state machine
  when(cmd.fire()) {
    when(doSetLength) {
      length := cmd.bits.rs1
    }.elsewhen(doCompress) {
      busy := true.B
      state := sLookForMatch
    }.elsewhen(doUncompress) {
      busy := true.B
      // ...
    }
  }

  //state machine for compress
  when(state === sLookForMatch) {

  }.elsewhen(state === sEmitLiteral) {

  }.otherwise/*(state === sIdle)*/ {

  }


  io.mem.req.valid := false.B
  io.resp.valid := false.B
  io.resp.bits.rd := io.resp.bits.rd
  io.resp.bits.data := SInt(-1, xLen).asUInt()


  io.cmd.ready := !busy
  io.busy := busy
  io.interrupt := false.B
  */

  val req_addr = Reg(UInt(width = coreMaxAddrBits))
  val req_rd = Reg(io.resp.bits.rd)
  val req_offset = req_addr(pgIdxBits - 1, 0)
  val req_vpn = req_addr(coreMaxAddrBits - 1, pgIdxBits)
  val pte = Reg(new PTE)

  val s_idle :: s_ptw_req :: s_ptw_resp :: s_resp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  io.cmd.ready := (state === s_idle)

  when (io.cmd.fire()) {
    req_rd := io.cmd.bits.inst.rd
    req_addr := io.cmd.bits.rs1
    state := s_ptw_req
  }

  private val ptw = io.ptw(0)

  when (ptw.req.fire()) { state := s_ptw_resp }

  when (state === s_ptw_resp && ptw.resp.valid) {
    pte := ptw.resp.bits.pte
    state := s_resp
  }

  when (io.resp.fire()) { state := s_idle }

  ptw.req.valid := (state === s_ptw_req)
  ptw.req.bits.valid := true.B
  ptw.req.bits.bits.addr := req_vpn

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := Mux(pte.leaf(), Cat(pte.ppn, req_offset), SInt(-1, xLen).asUInt)

  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)


}

object Hash {
  def apply(bytes: UInt, shift: UInt):UInt = {
    (bytes * 0x1e35a7bd.U >> shift).asUInt()
  }
}