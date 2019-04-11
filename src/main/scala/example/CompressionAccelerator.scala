package example

import chisel3._
import chisel3.core.dontTouch
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
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
  val ip        = RegInit( 0.U(32.W))
  val op        = RegInit( 0.U(32.W))
  val n         = RegInit( 0.U(32.W))
  val base      = RegInit( 0.U(32.W))
  val count     = RegInit( 0.U(32.W))
  val prev_ip   = RegInit( 0.U(32.W))
  dontTouch(ip_end)
  dontTouch(base_ip)
  dontTouch(next_emit)
  dontTouch(ip_limit)
  dontTouch(ip)
  dontTouch(op)
  dontTouch(n)
  dontTouch(base)
  dontTouch(count)
  dontTouch(prev_ip)






  // this is poked via treadle
  val fakeScratchpad = Mem(params.scratchpadEntries, UInt(params.scratchpadWidth.W))
  loadMemoryFromFile(fakeScratchpad, "memdata/memdata_1.txt")





//  val matchFinder = Module(new MatchFinder(params.scratchpadWidth, 32, params.hashTableSize))
//  matchFinder.io.start := startLooking
//  matchFinder.io.basePointer := basePointer
//  matchFinder.io.newCandidateData := fakeScratchpad.read(matchFinder.io.memoryReadAddress)
//   := matchFinder.io.matchFound
//   := matchFinder.io.matchBegin
//   := matchFinder.io.matchEnd




  // hold the lowest address in the hash table
  // TODO: is this necessary? If so, how do we accomplish it?
  val oldestInput = RegInit(0.U(32.W))

  // keep track of the range that the scratchpad contains
  val minScratchpadAddress = RegInit(0.U(32.W))
  val maxScratchpadAddress = RegInit(0.U(32.W))

  //TODO: make sure not to read and write the scratchpad at the same time if it's empty or full

  // bookkeeper for the dma side of the scratchpad
  val scratchpadBufferController = Module(new CircularBuffer(params.scratchpadEntries, params.scratchpadEntryBits))
  scratchpadBufferController.io.read := false.B //todo
  scratchpadBufferController.io.write := false.B
  scratchpadIO.dma.req.noenq()





  /*
  // when the scratchpad is not full, make a dma request
  when(!scratchpadBufferController.io.full && scratchpadIO.dma.req.ready){
    //TODO: make sure there are no bugs where we overwrite something (keep the min and max pointers in line)
    scratchpadIO.dma.req.enq(DMAUtils.makeDMARequest(write = false.B, maxScratchpadAddress, scratchpadBufferController.io.tail)(p, params))
  }

  // handle dma responses to the scratchpad
  when(scratchpadIO.dma.resp.valid) {
    when(scratchpadIO.dma.resp.bits.error) {
      printf("DMA returned error=true in a response (page fault?)\n") //TODO: figure out how to handle the error
    }.otherwise {
      maxScratchpadAddress := maxScratchpadAddress + 8.U
      scratchpadBufferController.io.write := true.B
    }
  }
  */




  // initialize each operation
  when(cmd.fire()) {
    when(doSetLength) {
      length := cmd.bits.rs1
    }.elsewhen(doCompress) {
      minScratchpadAddress := cmd.bits.rs1
      maxScratchpadAddress := cmd.bits.rs1
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
  io.resp.valid := false.B
  io.resp.bits.rd := RegNext(io.resp.bits.rd)
  io.resp.bits.data := (-1).S(xLen.W).asUInt()
  io.interrupt := false.B
}

object DMAUtils {
  def makeDMARequest(write: Bool, virtualAddress: UInt, scratchpadAddress: UInt)
                    (implicit p: Parameters, params: CompressionParameters): ScratchpadMemRequest = {
    val req = Wire(new ScratchpadMemRequest(params.scratchpadBanks, params.scratchpadEntries))
    req.vaddr := virtualAddress
    req.spbank := 0.U
    req.spaddr := scratchpadAddress
    req.write := write
    req
  }
}