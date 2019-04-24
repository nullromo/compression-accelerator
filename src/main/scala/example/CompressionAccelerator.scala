package example

import chisel3._
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
  scratchpadBanks = 2,
  scratchpadEntries = 128,
  scratchpadWidth = 64)

class CompressionAccelerator(opcodes: OpcodeSet, params: CompressionParameters = DefaultCompressionParameters)(implicit p: Parameters)
  extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new CompressionAcceleratorModule(this, params)
  val scratchpad = LazyModule(new Scratchpad(params.scratchpadBanks, params.scratchpadEntries, params.scratchpadWidth))
  val memoryctrl = LazyModule(new MemoryController(params.scratchpadEntries, params.scratchpadWidth))
  override val tlNode: TLIdentityNode = scratchpad.node
}

class CompressionAcceleratorModule(outer: CompressionAccelerator, params: CompressionParameters)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer) with HasCoreParameters {

  // get a reference to the scratchpad inside the implementation module
  import outer.{memoryctrl, scratchpad}
  val scratchpadIO = scratchpad.module.io
  val memoryctrlIO: MemoryControllerIO = memoryctrl.module.io

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

  // constants for compression
  val kInputMarginBytes: UInt = 15.U
  val inputEnd: UInt = src + length
  val inputLimit: UInt = inputEnd - kInputMarginBytes










  // adapter to read the scratchpad byte-by-byte in 32-bit chunks
  val aligner = Module(new MemoryReadAligner(
    32, 32, 32, 64
  ))

  // connect the aligner to the memory
  scratchpadIO.read(0)(0).addr := aligner.io.memIO.address
  aligner.io.memIO.data := scratchpadIO.read(0)(0).data

  // addresses sent to the aligner are always valid, but the aligner may choose not to be ready
  aligner.io.readIO.address.valid := true.B

  // the memory is always ready to be used by the aligner, but the aligner may not always be valid
  aligner.io.readIO.data.ready := true.B

  // tells the match finder where to begin a sequence
  val matchSearchAddress = RegInit(0.U(32.W))

  // pointer to tell the match finder where to start looking
  val basePointer = RegInit(0.U(32.W))

  // scans the scratchpad for matches
  val matchFinder = Module(new MatchFinder(params.scratchpadWidth, 32, params.hashTableSize))

  // pass in the global src pointer
  matchFinder.io.globalBase := src

  //
  matchFinder.io.start.bits := matchSearchAddress

  matchFinder.io.newCandidateData <> aligner.io.readIO.data
  aligner.io.readIO.address <> matchFinder.io.memoryReadAddress



  matchFinder.io.start.valid := ???






  // keep track of the range that the scratchpad contains
  val minScratchpadAddress = RegInit(0.U(32.W))
  val maxScratchpadAddress = RegInit(0.U(32.W))

  // everything between src and nextEmit has been accounted for in the output
  val nextEmit = RegInit(0.U(32.W))

  //

  memoryctrlIO.readBaseAddr := src
  memoryctrlIO.writeBaseAddr := dst
  memoryctrlIO.length := length
  memoryctrlIO.busy := busy











  // initialize each operation
  when(cmd.fire()) {
    when(doSetLength) {
        length := cmd.bits.rs1
    }.elsewhen(doCompress) {
        minScratchpadAddress := cmd.bits.rs1
        maxScratchpadAddress := cmd.bits.rs1
        busy := true.B
        src := cmd.bits.rs1
        dst := cmd.bits.rs2
    }.elsewhen(doUncompress) {
        busy := true.B
        // ...
    }
  }

  matchFinder.io.clear := cmd.fire()

  //TODO: figure out how to use these properly
  io.mem.req.valid := false.B
  io.resp.valid := false.B
  io.resp.bits.rd := RegNext(io.resp.bits.rd)
  io.resp.bits.data := (-1).S(xLen.W).asUInt()
  io.interrupt := false.B
}

object DMAUtils {
  def makeDMARequest(write: Bool, virtualAddress: UInt, scratchpadAddress: UInt)(implicit p: Parameters, params: CompressionParameters): ScratchpadMemRequest = {
    val req = Wire(new ScratchpadMemRequest(params.scratchpadBanks, params.scratchpadEntries))
    req.vaddr := virtualAddress
    req.spbank := 0.U
    req.spaddr := scratchpadAddress
    req.write := write
    req
  }
}
