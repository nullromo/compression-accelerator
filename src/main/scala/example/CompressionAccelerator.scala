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

    // valid match found
    val realMatchFound = Wire(Bool())
    // remain logic
    val remain = Wire(UInt(64.W))
    // prev_copyBusy
    val prev_copyBusy = RegInit(Bool())


    /*
    * Main pointers
    */
    // output pointer
    val scratchpadWriteAddress = RegInit(0.U(32.W))
    // keep track of the range that the scratchpad contains
    val minScratchpadAddress = RegInit(0.U(32.W))
    val maxScratchpadAddress = RegInit(0.U(32.W))
    // everything between src and nextEmit has been accounted for in the output
    val nextEmit = RegInit(0.U(32.W))
    val nextEmitValid = RegInit(false.B)
    // two pointers for the matches
    val matchA = RegInit(0.U(32.W))
    val matchB = RegInit(0.U(32.W))
    val offset = RegInit(0.U(32.W))

    /*
    * Read aligners
    */
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
    // the memory is always ready to be used by the aligners, but the aligners may not always be valid
    alignerA.io.readIO.data.ready := true.B
    alignerB.io.readIO.data.ready := true.B
    // connect the aligner addresses
    alignerA.io.readIO.address.bits := matchA
    alignerB.io.readIO.address.bits := matchB
    // addresses sent to the aligners are always valid, but the aligners may choose not to be ready
    alignerA.io.readIO.address.valid := true.B
    alignerB.io.readIO.address.valid := true.B

    aligner.io.readDataIO.data.ready := matchFinder.io.newData.ready || copyEmitter.io.data.forall((a) => a.ready === true.B)

    /*
    * Match finder
    */
    // scans the scratchpad for matches
    val matchFinder = Module(new MatchFinder(params.scratchpadWidth, 32, params.hashTableSize))
    // instantiate the module that does the copy length check
    val copyEmitter = Module(new CopyCompress(new CopyCompressParams{val parallellane = 4}))

    // pass in the global src pointer
    matchFinder.io.src := src
    // pass in the global matchB pointer
    matchFinder.io.matchB := matchB
    // clear the hash table when a new command is run
    matchFinder.io.clear := cmd.fire()
    // connect the matchFinder to the scratchpad, datawise
    // Match finder data valid is 1) when memory scratch pad is ready to read, 2) remain is large then 4, otherwise directly copy back
    //                            3) alignerdata is valid
    matchFinder.io.newData.valid := memoryctrlIO.readScratchpadReady && (remain >= 4.U) && aligner.io.readDataIO.data.valid
    matchFinder.io.newData.bits := aligner.io.readDataIO.data.bits
    // tell the matchFinder to start looking: when scratchpad is ready to read and copy emitter is just not busy
    matchFinder.io.start.valid := memoryctrlIO.readScratchpadReady && (~copyEmitter.io.copyBusy || (copyEmitter.io.copyBusy && copyEmitter.io.copyCompressed.valid))
    matchFinder.io.start.bits := DontCare

    /*
    * Copy emitter
    */
    // send the comparison data into the copyEmitter
    copyEmitter.io.candidate := aligner.io.readCandidateIO.data.asTypeOf(Vec(4, UInt(8.W)))
    copyEmitter.io.data := aligner.io.readDataIO.data.asTypeOf(Vec(4, UInt(8.W)))
    copyEmitter.io.offset := offset
    copyEmitter.io.valid := DontCare
    copyEmitter.io.hit := realMatchFound
    copyEmitter.io.remain := remain

    /* 
    * memory controller
    */
    memoryctrlIO.readBaseAddr := src
    memoryctrlIO.writeBaseAddr := dst
    memoryctrlIO.length := length
    memoryctrlIO.busy := busy
    memoryctrlIO.matchA := matchB   // should be reverse matchB is matchA and matchA is match B
    memoryctrlIO.matchB := matchA
    memoryctrlIO.nextEmit.bits := nextEmit
    memoryctrlIO.nextEmit.valid := nextEmitValid
    memoryctrlIO.emitEmptyBytePos := nextEmit
    memoryctrlIO.emitEmptyBytePos.valid := nextEmitValid
    memoryctrlIO.matchFound := matchFinder.io.matchA.valid
    memoryctrlIO.equal := copyEmitter.io.equal
    // -- encode end when : in literal mode, remain is 0; in copy mode, copy is not busy anymore
    memoryctrlIO.endEncode := (remain === 0.U) && (~copyEmitter.io.copyBusy)

    // when stream is true, bytes read from the read bank will be sent into the write bank
    val stream = RegInit(true.B)

    // stream searched bytes through to the write bank
    when(stream) {
        scratchpadIO.write(1).en := true.B
    }

    //TODO: when do we use which output?
    //TODO: deal with copyCompressed.bits.tag
        scratchpadIO.write(1).data := Mux(???, alignerB.io.readIO.data, copyEmitter.io.copyCompressed.bits.copy)


    // ****** Change of matchB (which is dataPtr) ******
    // -- when the system is finding match, add skip byte number when match finder is ready to receive data and dataPtr is within range
    // -- when the system is finding copy, add bufferIncPtr.bits every cycle when valid and dataPtr is within range
    // -- matchA and matchB are not reading address in scratchpad
    // ****** Change of MatchA (which is candidatePtr) ******
    // -- when match found, matchA should be the output of matchfinder
    // ----- need to check whether the hit is valid or not: if address < minvAddr + 8 (to keep aligner and controller working properly, two lines cannot be used)
    // -- when finding copy, matchA should be increased the same way as dataPtr

    realMatchFound := matchFinder.io.matchA.valid && (matchFinder.io.matchA.bits >= scratchpadIO.minvAddr + 8.U)
    prev_copyBusy := copyEmitter.io.copyBusy

    when(!copyEmitter.io.copyBusy) {
        when(matchFinder.io.newData.ready && !scratchpadIO.outOfRangeFlag){
            when(!realMatchFound) {
                matchB := matchB + 1.U // can be changed to skip later, also when match found, matchB should move + 4
            }
            .otherwise{
                matchB := matchB + 4.U // beacause at least 4 bytes should be the same
            }
        }
        when(realMatchFound) {
            matchA := matchFinder.io.matchA.bits + 4.U
            offset := matchB - matchA // offset logic
        }
    }
    .otherwise{
        when(copyEmitter.io.bufferIncPtr.valid && !scratchpadIO.outOfRangeFlag){
            matchB := matchB + copyEmitter.io.bufferIncPtr.bits
            matchA := matchA + copyEmitter.io.bufferIncPtr.bits
        }
    }

    // ****** remain logic *******
    remain = length - (matchB - minScratchpadAddress)

    // ****** next emit logic ******
    // -- nextEmit should be the matchB position when system just finish copy emitter
    // -- nextEmit valid when the literal emmitter finsh current match found and store them back in to write bank
    when(~copyEmitter.io.copyBusy && prev_copyBusy){
        nextEmit := matchB
        nextEmitValid := true.B
    }

    when(matchFinder.io.matchA.fire() && nextEmitValid){
        nextEmitValid := false.B
    }









    // initialize each operation
    when(cmd.fire()) {
        when(doSetLength) {
            length := cmd.bits.rs1
        }.elsewhen(doCompress) {
            minScratchpadAddress := cmd.bits.rs1
            maxScratchpadAddress := cmd.bits.rs1
            nextEmit := cmd.bits.rs1
            nextEmitValid := true.B
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
