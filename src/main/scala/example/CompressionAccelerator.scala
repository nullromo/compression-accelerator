package example

import chisel3._
import chisel3.core.dontTouch
import chisel3.util._
import external.{FrontendTLB, Scratchpad, ScratchpadMemRequest}
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
  val scratchpadIO = scratchpad.module.io

  // connect the scratchpad to the L2 cache
  implicit val edge: TLEdgeOut = outer.tlNode.edges.out.head
  val tlb = Module(new FrontendTLB(1, 4))
  tlb.io.clients(0) <> scratchpadIO.tlb
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
  val ip_end    = RegInit( 0.U(32.W))
  val base_ip   = RegInit( 0.U(32.W))
  val next_emit = RegInit( 0.U(32.W))
  val kInputMarginBytes: UInt = 15.U
  val ip_limit  = RegInit( 0.U(32.W))
  val skip      = RegInit(32.U(32.W))
  val ip        = RegInit( 0.U(32.W))
  val candidate = RegInit( 0.U(32.W))
  val op        = RegInit( 0.U(32.W))
  val n         = RegInit( 0.U(32.W))
  val base      = RegInit( 0.U(32.W))
  val count     = RegInit( 0.U(32.W))
  val prev_ip   = RegInit( 0.U(32.W))
  dontTouch(ip_end)
  dontTouch(base_ip)
  dontTouch(next_emit)
  dontTouch(ip_limit)
  dontTouch(skip)
  dontTouch(ip)
  dontTouch(candidate)
  dontTouch(op)
  dontTouch(n)
  dontTouch(base)
  dontTouch(count)
  dontTouch(prev_ip)

  // define bytes between hash lookups for searching for matches
  val bbhl: UInt = Wire(UInt(32.W))
  bbhl := (skip >> 5.U).asUInt()
  dontTouch(bbhl)

  // create the hash table
  val hashTable = Module(new HashTable(32, 16, params.hashTableSize))

  // values coming out of the hash table
  val oldCandidateData: UInt = Wire(UInt(32.W))
  val oldCandidateOffset: UInt = Wire(UInt(16.W))

  // values going into the hash table
  val newCandidateData: UInt = Wire(UInt(32.W))
  val newCandidateOffset: UInt = Wire(UInt(16.W))
  newCandidateData := scratchpadIO.read(0)(0).data
  newCandidateOffset := ip - base_ip

  // connect hash table
  hashTable.io.newData := newCandidateData
  hashTable.io.newOffset := newCandidateOffset
  oldCandidateData := hashTable.io.oldData
  oldCandidateOffset := hashTable.io.oldOffset

  //TODO: this will have some more control logic
  hashTable.io.enable := true.B

  // ture when a match has been found
  val matchFound: Bool = Wire(Bool())
  matchFound := oldCandidateData === newCandidateData




  prev_ip := ip
  skip := skip + bbhl
  ip := ip + bbhl



  // hold the lowest address in the hash table
  // TODO: is this necessary? If so, how do we accomplish it?
  val oldestInput = RegInit(0.U(32.W))

  // keep track of the range that the scratchpad contains
  val minScratchpadAddress = RegInit(0.U(32.W))
  val maxScratchpadAddress = RegInit(0.U(32.W))

  //TODO: make sure not to read and write the scratchpad at the same time if it's empty or full

  // bookkeeper for the dma side of the scratchpad
  val scratchpadBufferController = Module(new CircularBuffer(params.scratchpadEntries, params.scratchpadWidth))
  scratchpadBufferController.io.read := false.B //todo

  // create a buffer of outstanding dma requests
  val dmaRequest = new ScratchpadMemRequest(params.scratchpadBanks, params.scratchpadEntries)
  val dmaRequestBuffer = Queue(Decoupled(dmaRequest))
  dmaRequestBuffer.noenq()

  // always send the buffered requests
  scratchpadIO.dma.req.bits := dmaRequestBuffer.deq()

  // if the scratchpad is not full, fill it up by generating a new dma request
  when(!scratchpadBufferController.io.full) {
    //TODO: make sure there are no bugs where we overwrite something
    dmaRequest := DMAUtils.makeDMARequest(false.B, maxScratchpadAddress, scratchpadBufferController.tail)(p, params)
    dmaRequestBuffer.enq(dmaRequest)
    maxScratchpadAddress := maxScratchpadAddress + 8.U
    scratchpadBufferController.io.write := true.B
  }







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

object DMAUtils {
  def makeDMARequest(write: Bool, virtualAddress: UInt, scratchpadAddress: UInt)(implicit p: Parameters, params: CompressionParameters): ScratchpadMemRequest = {
    val req = new ScratchpadMemRequest(params.scratchpadBanks, params.scratchpadEntries)
    req.vaddr := virtualAddress
    req.spbank := 0.U
    req.spaddr := scratchpadAddress
    req.write := write
    req
  }
}