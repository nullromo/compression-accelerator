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

    // valid match found (matchA is in valid scratchpad look-back range)
    val realMatchFound: Bool = Wire(Bool())
    // keep track of how many bytes there are left to compress
    val remain: UInt = Wire(UInt(64.W))
    // true when the copy path was working on the previous cycle
    val prev_copyBusy = RegInit(false.B)
    // true when the matchFinder was ready to start on the previous cycle
    val prev_startReady = RegInit(true.B)
    // true when an emit was forced on the previous cycle
    val prev_forceEmit = RegInit(false.B)

    // keeps track of how many things are being copied to the write bank
    val streamCounter = RegInit(0.U(4.W))
    // buffer for data being copied to write bank
    val streamHolder = RegInit(VecInit(Seq.fill(16)(0.U(8.W))))
    val streamEmpty = RegInit(false.B)
    // flag for when the length of a literal goes to 60
    val forceEmit: Bool = Wire(Bool())
    // for the empty literal length slot, this is the offset within the stream buffer
    val emptySpotCounter = RegInit(0.U(3.W))
    // holds the actual address of the empty slot so that it can be filled in later
    val emptySpotAddr = RegInit(0.U(log2Ceil(params.scratchpadEntries).W))

    // everything between src and nextEmit has been accounted for in the output
    val nextEmit = RegInit(0.U(32.W))
    val nextEmitValid = RegInit(false.B)
    // two pointers for the matches
    val matchA = RegInit(0.U(32.W))
    val matchB = RegInit(0.U(32.W))
    // distance between matchA and matchB
    val offset = RegInit(0.U(32.W))

    /*
    * Read aligner
    */
    // adapter to read the scratchpad byte-by-byte in 32-bit chunks
    val aligner = Module(new MemoryReadAligner(
        32, 32, 32, 64
    ))
    // connect the aligners to the memory
    scratchpadIO.read(0)(0).addr := (aligner.io.memDataIO.address - src) % (params.scratchpadEntries * params.scratchpadWidth / 8).U
    scratchpadIO.read(0)(1).addr := (aligner.io.memCandidateIO.address - src) % (params.scratchpadEntries * params.scratchpadWidth / 8).U
    scratchpadIO.read(0)(0).en := aligner.io.memDataIO.en
    scratchpadIO.read(0)(1).en := aligner.io.memCandidateIO.en
    aligner.io.memDataIO.data := scratchpadIO.read(0)(0).data
    aligner.io.memCandidateIO.data := scratchpadIO.read(0)(1).data
    // the memory is always ready to be used by the aligners, but the aligners may not always be valid
    aligner.io.readDataIO.data.ready := true.B
    aligner.io.readCandidateIO.data.ready := true.B
    // connect the aligner addresses
    aligner.io.readDataIO.address.bits := matchB
    aligner.io.readCandidateIO.address.bits := matchA
    // addresses sent to the aligners are always valid, but the aligners may choose not to be ready
    aligner.io.readDataIO.address.valid := !memoryctrlIO.readScratchpadReady
    aligner.io.readCandidateIO.address.valid := !memoryctrlIO.readScratchpadReady

    aligner.io.equal := true.B //TODO: what does this signal do?

    /*
    * Match finder
    */
    // scans the scratchpad for matches
    val matchFinder = Module(new MatchFinder(params.scratchpadWidth, 32, params.hashTableSize))
    // instantiate the module that does the copy length check
    val copyEmitter = Module(new CopyCompress(new CopyCompressParams {
        val parallellane = 4
    }))

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
    matchFinder.io.start.valid := memoryctrlIO.readScratchpadReady && (!copyEmitter.io.copyBusy || (copyEmitter.io.copyBusy && copyEmitter.io.copyCompressed.valid))
    matchFinder.io.start.bits := DontCare
    forceEmit := ((matchB - nextEmit) > 59.U)
    matchFinder.io.matchA.ready := true.B

    /*
    * Copy emitter
    */
    // send the comparison data into the copyEmitter
    (copyEmitter.io.candidate zip aligner.io.readCandidateIO.data.bits.asTypeOf(Vec(4, UInt(8.W)))).foreach { case (cand, aio) => cand.bits := aio }
    (copyEmitter.io.data zip aligner.io.readDataIO.data.bits.asTypeOf(Vec(4, UInt(8.W)))).foreach { case (data, aio) => data.bits := aio }
    when(memoryctrlIO.readScratchpadReady && aligner.io.readDataIO.data.valid) {
        (copyEmitter.io.candidate zip copyEmitter.io.data).zipWithIndex.foreach({
            case ((a, b), i) =>
                a.valid := Mux(i.U < remain || remain >= 4.U, true.B, false.B)
                b.valid := Mux(i.U < remain || remain >= 4.U, true.B, false.B)
        })
    }.otherwise {
        (copyEmitter.io.candidate zip copyEmitter.io.data).foreach({
            case (a, b) =>
                a.valid := false.B
                b.valid := false.B
        })
    }
    copyEmitter.io.offset.bits := offset
    copyEmitter.io.offset.valid := true.B //TODO: is this right? Probably not.
    copyEmitter.io.hit := realMatchFound
    copyEmitter.io.remain := remain
    copyEmitter.io.bufferPtrInc.ready := true.B
    copyEmitter.io.copyCompressed.ready := true.B

    /*
    * memory controller
    */
    memoryctrlIO.readBaseAddr := src
    memoryctrlIO.writeBaseAddr := dst
    memoryctrlIO.length := length
    memoryctrlIO.busy := busy
    memoryctrlIO.matchB := (matchB - src) % (params.scratchpadEntries * params.scratchpadWidth / 8).U // should be reverse matchB is matchA and matchA is match B
    memoryctrlIO.matchA := (matchA - src) % (params.scratchpadEntries * params.scratchpadWidth / 8).U
    memoryctrlIO.nextEmit.bits := nextEmit
    memoryctrlIO.nextEmit.valid := nextEmitValid
    memoryctrlIO.emitEmptyBytePos.bits := emptySpotAddr * 8.U + emptySpotCounter
    memoryctrlIO.emitEmptyBytePos.valid := nextEmitValid
    memoryctrlIO.matchFound := realMatchFound
    memoryctrlIO.equal := copyEmitter.io.equal
    // -- encode end when : in literal mode, remain is 0; in copy mode, copy is not busy anymore
    memoryctrlIO.endEncode := (remain === 0.U) && (!copyEmitter.io.copyBusy)
    memoryctrlIO.storeData.valid := !memoryctrlIO.fullSW && (streamCounter > 7.U) /// needs to check !!!!!!!!!!!!
	scratchpadIO.dma <> memoryctrlIO.dma

    // when stream is true, bytes read from the read bank will be sent into the write bank
    val stream = RegInit(true.B)

    // stream searched bytes through to the write bank
    scratchpadIO.write(1).en := ((streamCounter > 7.U) || forceEmit) && !memoryctrlIO.fullSW
    scratchpadIO.write(1).data := Mux(forceEmit || matchFinder.io.matchA.valid, ((matchB - nextEmit) << 2.U).asTypeOf(UInt(64.W)), streamHolder.asTypeOf(UInt(128.W))(63, 0))
    scratchpadIO.write(1).addr := Mux(forceEmit || matchFinder.io.matchA.valid, emptySpotAddr, memoryctrlIO.storeSpAddr)
    scratchpadIO.write(1).mask := Mux(forceEmit || matchFinder.io.matchA.valid, (1.U << emptySpotCounter).asTypeOf(Vec(8, Bool())), 255.U.asTypeOf(Vec(8, Bool())))

    // how many bytes are outputted as the copy based on the tag type
    // 00 => 0 bytes; 01 => 2 bytes; 10 => 3 bytes; 11 => 5 bytes
    val bytesInCopyTag: UInt = Wire(UInt(3.W))
    bytesInCopyTag := VecInit(0.U, 2.U, 3.U, 5.U)(copyEmitter.io.copyCompressed.bits.tag)

    // Increase stream counter
    when(!memoryctrlIO.fullSW) {
        when(!matchFinder.io.start.ready) {
            when(prev_startReady || (!forceEmit && prev_forceEmit)) {
                when(aligner.io.readDataIO.data.valid) {
                    streamCounter := (streamCounter % 8.U) + 2.U
                    streamHolder((streamCounter % 8.U) + 1.U) := aligner.io.readDataIO.data.bits(7, 0)
                }.otherwise {
                    streamCounter := (streamCounter % 8.U) + 1.U
                }
            }.otherwise {
                streamCounter := (streamCounter % 8.U) + 1.U
                streamHolder(streamCounter % 8.U) := aligner.io.readDataIO.data.bits(7, 0)
            }
        }.elsewhen(copyEmitter.io.copyCompressed.fire()) {
            streamCounter := (streamCounter % 8.U) + bytesInCopyTag
            for (i <- 0 until 5) {
                when(i.U < bytesInCopyTag) {
                    streamHolder((streamCounter % 8.U) + i.U) := copyEmitter.io.copyCompressed.bits.copy((i * 8) + 7, i * 8)
                }
            }
        }

        // shift stream holder when a cache line is full
        when(streamCounter > 7.U) {
            (streamHolder.slice(0, 8) zip streamHolder.slice(8, 16)).foreach { case (a, b) => a := b }
            streamHolder.slice(8, 16).foreach(a => a := 0.U)
        }
    }


    // ****** Change of matchB (which is dataPtr) ******
    // -- when the system is finding match, add skip byte number when match finder is ready to receive data and dataPtr is within range
    // -- when the system is finding copy, add bufferIncPtr.bits every cycle when valid and dataPtr is within range
    // -- matchA and matchB are not reading address in scratchpad
    // ****** Change of MatchA (which is candidatePtr) ******
    // -- when match found, matchA should be the output of matchfinder
    // ----- need to check whether the hit is valid or not: if address < minvAddr + 8 (to keep aligner and controller working properly, two lines cannot be used)
    // -- when finding copy, matchA should be increased the same way as dataPtr

    realMatchFound := matchFinder.io.matchA.valid && (matchFinder.io.matchA.bits >= memoryctrlIO.minvAddr + 8.U)
    prev_copyBusy := copyEmitter.io.copyBusy
    prev_startReady := matchFinder.io.start.ready
    prev_forceEmit := forceEmit

    when(!copyEmitter.io.copyBusy && memoryctrlIO.readScratchpadReady) {
        when(matchFinder.io.newData.ready && !memoryctrlIO.outOfRangeFlag) {
            when(!realMatchFound) {
                matchB := matchB + 1.U // can be changed to skip later, also when match found, matchB should move + 4
            }.otherwise {
                matchB := matchB + 4.U // beacause at least 4 bytes should be the same
            }
        }
        when(realMatchFound) {
            matchA := matchFinder.io.matchA.bits + 4.U
            offset := matchB - matchA // offset logic
        }
    }.elsewhen(memoryctrlIO.readScratchpadReady) {
        when(copyEmitter.io.bufferPtrInc.valid && !memoryctrlIO.outOfRangeFlag) {
            matchB := matchB + copyEmitter.io.bufferPtrInc.bits
            matchA := matchA + copyEmitter.io.bufferPtrInc.bits
        }
    }

    // ****** remain logic *******
    remain := length - (matchB - src)

    // ****** next emit logic ******
    // -- nextEmit should be the matchB position when system just finish copy emitter
    // -- nextEmit valid when the literal emitter finish current match found and store them back into write bank
    when((!copyEmitter.io.copyBusy && prev_copyBusy) || forceEmit) {
        nextEmit := matchB
        nextEmitValid := true.B
        emptySpotAddr := memoryctrlIO.storeSpAddr + (streamCounter / 8.U)
        emptySpotCounter := streamCounter % 8.U
    }

    when(matchFinder.io.matchA.fire() && nextEmitValid) {
        nextEmitValid := false.B
    }


    // initialize each operation
    when(cmd.fire()) {
        when(doSetLength) {
            length := cmd.bits.rs1
        }.elsewhen(doCompress) {
            nextEmit := cmd.bits.rs1
            nextEmitValid := true.B
            matchA := cmd.bits.rs1
            matchB := cmd.bits.rs1
            matchFinder.io.start.valid := true.B
            busy := true.B
            src := cmd.bits.rs1
            dst := cmd.bits.rs2
            streamCounter := 0.U
            streamHolder.foreach(_ := 0.U)
            streamEmpty := true.B
            prev_startReady := true.B
            prev_forceEmit := false.B
        }.elsewhen(doUncompress) {
            busy := true.B
            // ...
        }
    }

    dontTouch(nextEmit)
    dontTouch(nextEmitValid)
    dontTouch(dst)
    dontTouch(streamCounter)
    dontTouch(streamHolder)
    dontTouch(streamEmpty)
    dontTouch(scratchpadIO)

    matchFinder.io.clear := cmd.fire()

    //TODO: figure out how to use these properly
    io.mem.req.valid := false.B
    io.resp.valid := false.B
    io.resp.bits.rd := RegNext(io.resp.bits.rd)
    io.resp.bits.data := (-1).S(xLen.W).asUInt()
    io.interrupt := false.B
}