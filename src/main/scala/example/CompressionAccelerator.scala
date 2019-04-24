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










  // adapters to read the scratchpad byte-by-byte in 32-bit chunks
  val alignerA = Module(new MemoryReadAligner(
    32, 32, 32, 64
  ))
  val alignerB = Module(new MemoryReadAligner(
    32, 32, 32, 64
  ))

  // connect the aligners to the memory
  scratchpadIO.read(0)(0).addr := alignerA.io.memIO.address
  scratchpadIO.read(0)(1).addr := alignerB.io.memIO.address
  alignerA.io.memIO.data := scratchpadIO.read(0)(0).data
  alignerB.io.memIO.data := scratchpadIO.read(0)(1).data

  // addresses sent to the aligners are always valid, but the aligners may choose not to be ready
  alignerA.io.readIO.address.valid := true.B
  alignerB.io.readIO.address.valid := true.B

  // the memory is always ready to be used by the aligners, but the aligners may not always be valid
  alignerA.io.readIO.data.ready := true.B
  alignerB.io.readIO.data.ready := true.B

  // scans the scratchpad for matches
  val matchFinder = Module(new MatchFinder(params.scratchpadWidth, 32, params.hashTableSize))

  // pass in the global src pointer
  matchFinder.io.src := src

  // start looking for matches at matchB
  matchFinder.io.start.bits := matchB

  // connect the matchFinder to the scratchpad
  matchFinder.io.newCandidateData <> alignerB.io.readIO.data
  alignerB.io.readIO.address <> matchFinder.io.memoryReadAddress


  // instantiate the module that does the copy length check
  val copyEmitter = Module(new CopyCompress(new CopyCompressParams{val parallellane = 1}))

  // send the comparison data into the copyEmitter
  copyEmitter.io.candidate := alignerA.io.readIO.data.asTypeOf(Vec(4, UInt(8.W)))
  copyEmitter.io.data := alignerB.io.readIO.data.asTypeOf(Vec(4, UInt(8.W)))

  //
  alignerA.io.readIO.address := matchA

  /**
    * Start main controller work
    */


  matchFinder.io.start.valid := ???
  matchFinder.io.clear := false.B

  // output pointer
  val scratchpadWriteAddress = RegInit(0.U(32.W))

  // keep track of the range that the scratchpad contains
  val minScratchpadAddress = RegInit(0.U(32.W))
  val maxScratchpadAddress = RegInit(0.U(32.W))

  // everything between src and nextEmit has been accounted for in the output
  val nextEmit = RegInit(0.U(32.W))

  // two pointers for the matches
  val matchA = RegInit(0.U(32.W))
  val matchB = RegInit(0.U(32.W))

  memoryctrlIO.readBaseAddr := src
  memoryctrlIO.writeBaseAddr := dst
  memoryctrlIO.length := length
  memoryctrlIO.busy := busy
  memoryctrlIO.matchA := matchA
  memoryctrlIO.matchB := matchB
  memoryctrlIO.nextEmit := nextEmit
  memoryctrlIO.emitEmptyBytePos := ???

  // when stream is true, bytes read from the read bank will be sent into the write bank
  val stream = RegInit(true.B)

  // stream searched bytes through to the write bank
  when(stream) {
    scratchpadIO.write(1).en := true.B
    scratchpadIO.write(1).data := scratchpadIO.read(0)(0).data
  }

  // when a match is found, stop sending literal bytes to the output
  when(matchFinder.io.out.valid) {
    stream := false.B
  }.otherwise {

  }



  /**
    * End main controller work
    */








  // initialize each operation
  when(cmd.fire()) {
    when(doSetLength) {
        length := cmd.bits.rs1
    }.elsewhen(doCompress) {
        matchFinder.io.clear := true.B
        minScratchpadAddress := cmd.bits.rs1
        maxScratchpadAddress := cmd.bits.rs1
        nextEmit := cmd.bits.rs1
        matchA := cmd.bits.rs1
        matchB := cmd.bits.rs1 + 1.U
        matchFinder.io.start.valid := true.B
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
