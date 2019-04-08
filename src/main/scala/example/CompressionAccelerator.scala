package example

import chisel3._
import chisel3.core.dontTouch
import chisel3.util._
import external.{FrontendTLB, Scratchpad}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLEdgeOut, TLIdentityNode}

case class CompressionParameters(hashTableSize: Int,
                                 scratchpadBanks: Int,
                                 scratchpadEntries: Int,
                                 scratchpadWidth: Int) {
  val scratchpadEntryBits = log2Ceil(scratchpadEntries)
}

object DefaultCompressionParameters extends CompressionParameters(
  hashTableSize = 4096,
  scratchpadBanks = 1,
  scratchpadEntries = 4096,
  scratchpadWidth = 64)

class CompressionAccelerator(opcodes: OpcodeSet, params: CompressionParameters = DefaultCompressionParameters)(implicit p: Parameters)
  extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new CompressionAcceleratorModule(this, params)
  val scratchpad = LazyModule(new Scratchpad(params.scratchpadBanks, params.scratchpadEntries, params.scratchpadWidth))
  override val tlNode: TLIdentityNode = scratchpad.node
}

class CompressionAcceleratorModule(outer: CompressionAccelerator, params: CompressionParameters)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer) with HasCoreParameters {

  // get a reference to the scratchpad inside the implementation module
  import outer.scratchpad

  // connect the scratchpad to the L2 cache
  implicit val edge: TLEdgeOut = outer.tlNode.edges.out.head
  val tlb = Module(new FrontendTLB(1, 4))
  tlb.io.clients(0) <> scratchpad.module.io.tlb
  io.ptw.head <> tlb.io.ptw

  //TODO: change all the magic numbers to parameters

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
  cmd.ready := !busy
  io.busy := busy

  // components for state machine
  val shift: UInt = (32 - log2Floor(params.hashTableSize)).U
  val ip_end    = RegInit( 0.U(32.W))
  val base_ip   = RegInit( 0.U(32.W))
  val next_emit = RegInit( 0.U(32.W))
  val kInputMarginBytes: UInt = 15.U
  val ip_limit  = RegInit( 0.U(32.W))
  val hash      = RegInit( 0.U(32.W))
  val skip      = RegInit(32.U(32.W))
  val ip        = RegInit( 0.U(32.W))
  val candidate = RegInit( 0.U(32.W))
  val op        = RegInit( 0.U(32.W))
  val n         = RegInit( 0.U(32.W))
  val base      = RegInit( 0.U(32.W))
  val count     = RegInit( 0.U(32.W))
  val prev_ip   = RegInit( 0.U(32.W))
  val matched   = RegInit( 0.U(32.W))
  val s2        = RegInit( 0.U(32.W))
  val toCopy    = RegInit( 0.U(32.W))
  dontTouch(ip_end)
  dontTouch(base_ip)
  dontTouch(next_emit)
  dontTouch(ip_limit)
  dontTouch(hash)
  dontTouch(skip)
  dontTouch(ip)
  dontTouch(candidate)
  dontTouch(op)
  dontTouch(n)
  dontTouch(base)
  dontTouch(count)
  dontTouch(prev_ip)
  dontTouch(matched)
  dontTouch(s2)
  dontTouch(toCopy)

  // define bytes between hash lookups for searching for matches
  val bbhl: UInt = Wire(UInt(32.W))
  bbhl := (skip >> 5.U).asUInt()
  dontTouch(bbhl)

  // create the hash table and connect it
  val hashTable = Module(BasicMem(MemParameters(params.hashTableSize, 16, syncRead = false, bypass = false)))
  hashTable.io.writeAddress := hash
  hashTable.io.writeData := ip - base_ip
  hashTable.io.writeEnable := true.B
  hashTable.io.readAddress := hash
  candidate := hashTable.io.readData

  prev_ip := ip
  skip := skip + bbhl
  ip := ip + bbhl

  // index into hash table
  hash := Hash(ip + (skip >> 5.U).asUInt(), shift)



  // hold the lowest address in the hash table
  val oldestInput = RegInit(0.U(32.W))

  // keep track of the range that the scratchpad contains
  val minScratchpadAddress = RegInit(0.U(32.W))
  val maxScratchpadAddress = RegInit(0.U(32.W))

  // signals for scratchpad
  val addToScratchpad: Bool = Wire(Bool())
  val consumeFromScratchpad: Bool = Wire(Bool())

  val scratchpadBufferController = Module(new CircularBuffer(params.scratchpadEntries, params.scratchpadWidth))
  scratchpadBufferController.io.write := addToScratchpad
  scratchpadBufferController.io.read := consumeFromScratchpad
  //TODO: make sure not to read and write the scratchpad at the same time if it's empty or full

  when(!scratchpadBufferController.io.full) {
    //todo: make the read request
    scratchpad.module.io.dma.req.valid := true.B


    maxScratchpadAddress := maxScratchpadAddress + 8.U
  }



  //TODO: this is the goal
  //match = true if mem[ip] == mem[candidate]
  val matchFound: Bool = Wire(Bool())
  dontTouch(matchFound)

  matchFound := scratchpad.module.io.read(0)(0).data === scratchpad.module.io.read(0)(1).data





  // initialize each operation
  when(cmd.fire()) {
    when(doSetLength) {
      length := cmd.bits.rs1
    }.elsewhen(doCompress) {
      //TODO: some of the signals are redundant
      busy := true.B
      op := cmd.bits.rs2
      src := cmd.bits.rs1
      dst := cmd.bits.rs2
      ip_end := cmd.bits.rs1 + length
      base_ip := cmd.bits.rs1
      next_emit := cmd.bits.rs1
      ip_limit := cmd.bits.rs1 + length - kInputMarginBytes
      hash := Hash(cmd.bits.rs1 + 1.U, shift)
      ip := cmd.bits.rs1 + 1.U
    }.elsewhen(doUncompress) {
      busy := true.B
      // ...
    }
  }


  //TODO: figure out how to use these properly
  io.mem.req.valid := false.B
  io.resp.valid := candidate =/= 0.U
  io.resp.bits.rd := RegNext(io.resp.bits.rd)
  io.resp.bits.data := (-1).S(xLen.W).asUInt()
  io.interrupt := false.B
}

object Hash {
  def apply(bytes: UInt, shift: UInt): UInt = {
    (bytes * 0x1e35a7bd.U >> shift).asUInt()
  }
}