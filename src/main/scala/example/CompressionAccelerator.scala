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
  import outer.scratchpad
  import outer.memoryctrl
  val scratchpadIO = scratchpad.module.io
  val memoryctrlIO = memoryctrl.module.io

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
  //matchFound := oldCandidateData === newCandidateData

  // ----- TEST ScratchPad Settings ----------
  /*val writeScratchPadCounter = RegInit(100.U(8.W))
  val teststate = RegInit(0.U(3.W))
  val sInit_test = 0.U
  val sWriteScratchPad_test = 1.U
  val sWriteMem_test = 2.U
  val sReadMemReverse_test = 3.U
  val sReadScratchPad_test = 4.U
  val sDone_test = 5.U
  val scratchpadPtr = RegInit(0.U(32.W))*/

  // ------ TEST memory controller -----------
  val (s_idle :: s_fill :: s_nomatch :: s_write :: s_match ::
       s_stop_wait :: s_done :: Nil) = Enum(7)
  val teststate = RegInit(s_idle)
  val dataPtr = RegInit(0.U(log2Ceil(params.scratchpadEntries*8).W))
  val candidatePtr = RegInit(0.U(log2Ceil(params.scratchpadEntries*8).W))
  val equal = Wire(Bool())
  val endEncode = RegInit(false.B)
  val storeData_bits = RegInit(0.U((params.scratchpadWidth).W))
  val storeData_valid = RegInit(false.B)


  // -----------------------------------------
  


  prev_ip := ip
  skip := skip + bbhl
  ip := ip + bbhl



  // hold the lowest address in the hash table
  // TODO: is this necessary? If so, how do we accomplish it?
  val oldestInput = RegInit(0.U(32.W))

  // keep track of the range that the scratchpad contains
  val minScratchpadAddress = RegInit(0.U(32.W))
  val maxScratchpadAddress = RegInit(0.U(32.W))
  val minWriteAddress = RegInit(0.U(32.W))

  //TODO: make sure not to read and write the scratchpad at the same time if it's empty or full

  // bookkeeper for the dma side of the scratchpad
  val scratchpadBufferController = Module(new CircularBuffer(params.scratchpadEntries, params.scratchpadEntryBits))
  scratchpadBufferController.io.read := false.B //todo
  scratchpadBufferController.io.write := false.B
  //scratchpadIO.dma.req.noenq()

  scratchpadIO.dma.resp.ready := true.B
  // when the scratchpad is not full, make a dma request
  /*when(!scratchpadBufferController.io.full && busy && scratchpadIO.dma.req.ready){
    //TODO: make sure there are no bugs where we overwrite something (keep the min and max pointers in line)
    scratchpadIO.dma.req.bits := DMAUtils.makeDMARequest(write = false.B, maxScratchpadAddress, scratchpadBufferController.io.tail)(p, params)
	scratchpadIO.dma.req.valid := true.B
  }.otherwise{
	scratchpadIO.dma.req.bits := DMAUtils.makeDMARequest(write = false.B, maxScratchpadAddress, scratchpadBufferController.io.tail)(p, params)
  	scratchpadIO.dma.req.valid := false.B
  }

  // handle dma responses to the scratchpad
  when(scratchpadIO.dma.resp.valid && busy) {
    when(scratchpadIO.dma.resp.bits.error) {
      printf("DMA returned error=true in a response (page fault?)\n") //TODO: figure out how to handle the error
    }.otherwise {
      maxScratchpadAddress := maxScratchpadAddress + 8.U
      scratchpadBufferController.io.write := true.B
    }
  }


  // Testing whether Scratchpad fills in or not
  scratchpadIO.read.foreach{(a) => a(0).en := busy}
  scratchpadIO.read.foreach{(a) => a(0).addr := Mux(maxScratchpadAddress > 0.U, (maxScratchpadAddress-8.U) >> 3, 0.U)}	
  printf("data is: %d\n", scratchpadIO.read(0)(0).data)*/


  	// ------ TEST ScrathPad ------------------------
	/*scratchpadIO.write.foreach{(a) => a.en := teststate === sWriteScratchPad_test}
	scratchpadIO.write.foreach{(a) => a.addr := scratchpadPtr >> 3}
	scratchpadIO.write.foreach{(a) => a.data := scratchpadPtr}

	scratchpadIO.read.foreach{(a) => a(0).en := teststate === sReadScratchPad_test}
	scratchpadIO.read.foreach{(a) => a(0).addr := Mux(scratchpadPtr > 0.U, (scratchpadPtr-8.U) >> 3, 0.U)}
	

    scratchpadIO.dma.req.bits := DMAUtils.makeDMARequest(write = (teststate === sWriteMem_test), maxScratchpadAddress, scratchpadPtr >> 3)(p, params)
	scratchpadIO.dma.req.valid := ((teststate === sWriteMem_test || teststate === sReadMemReverse_test) && scratchpadIO.dma.req.ready)

    when(teststate === sInit_test){
		when(busy){
			teststate := sWriteScratchPad_test
		}	
	}
	.elsewhen(teststate === sWriteScratchPad_test){
		when(writeScratchPadCounter > 0.U){
			scratchpadPtr := scratchpadPtr + 8.U
			writeScratchPadCounter := writeScratchPadCounter - 1.U
		}
		.otherwise{
			teststate := sWriteMem_test
			writeScratchPadCounter := 100.U
			scratchpadPtr := 0.U
		}
	}
	.elsewhen(teststate === sWriteMem_test){
		when(writeScratchPadCounter > 0.U){
			when(scratchpadIO.dma.resp.valid){
				when(scratchpadIO.dma.resp.bits.error) {
			  		printf("DMA returned write error=true in a response (page fault?)\n") //TODO: figure out how to handle the error
				}.otherwise {
			  		maxScratchpadAddress := maxScratchpadAddress + 8.U
					scratchpadPtr := scratchpadPtr + 8.U
   				}
				writeScratchPadCounter := writeScratchPadCounter - 1.U
			}
		}
		.otherwise{
			when(scratchpadIO.dma.resp.valid){
				when(scratchpadIO.dma.resp.bits.error) {
			  		printf("DMA returned write error=true in a response (page fault?)\n") //TODO: figure out how to handle the error
				}.otherwise {
					scratchpadPtr := 0.U
   				}
				writeScratchPadCounter := 100.U
				teststate := sReadMemReverse_test
			}
		}
	}
	.elsewhen(teststate === sReadMemReverse_test){
		when(writeScratchPadCounter > 0.U){
			when(scratchpadIO.dma.resp.valid){
				when(scratchpadIO.dma.resp.bits.error) {
			  		printf("DMA returned read error=true in a response (page fault?)\n") //TODO: figure out how to handle the error
				}.otherwise {
			  		maxScratchpadAddress := maxScratchpadAddress - 8.U
					scratchpadPtr := scratchpadPtr + 8.U
   				}
				writeScratchPadCounter := writeScratchPadCounter - 1.U
			}
		}
		.otherwise{
			when(scratchpadIO.dma.resp.valid){
				when(scratchpadIO.dma.resp.bits.error) {
			  		printf("DMA returned read error=true in a response (page fault?)\n") //TODO: figure out how to handle the error
				}.otherwise {
					scratchpadPtr := 0.U
   				}
				writeScratchPadCounter := 100.U
				teststate := sReadScratchPad_test
			}
		}
	}
	.elsewhen(teststate === sReadScratchPad_test){
		when(writeScratchPadCounter > 0.U){
			scratchpadPtr := scratchpadPtr + 8.U
			writeScratchPadCounter := writeScratchPadCounter - 1.U
		}
		.otherwise{
			teststate := sDone_test
			writeScratchPadCounter := 100.U
			scratchpadPtr := 0.U
		}
		printf("data is: %d\n", scratchpadIO.read(0)(0).data)
	}
  */
	// -------------- Test Memory Controller ----------------------
    val counter = RegInit(0.U(7.W))
    val matchCounter = RegInit(0.U(7.W))
    memoryctrlIO.readBaseAddr := minScratchpadAddress
    memoryctrlIO.writeBaseAddr := minWriteAddress
    memoryctrlIO.length := length
    memoryctrlIO.busy := busy
    memoryctrlIO.dataPtr.bits := dataPtr
    memoryctrlIO.candidatePtr.bits := candidatePtr
    memoryctrlIO.matchFound := matchFound
    memoryctrlIO.equal := equal
    memoryctrlIO.endEncode := endEncode
 	scratchpadIO.dma <> memoryctrlIO.dma

    when(teststate === s_idle){
      when(memoryctrlIO.readScratchpadReady === true.B){
        teststate := s_nomatch
      }
    }
    .elsewhen(teststate === s_nomatch){
      counter := counter + 1.U
      when(counter === 2.U){
        dataPtr := 2.U
      }
      .elsewhen(counter === 6.U){
        dataPtr := 72.U
      }
      .elsewhen(counter === 10.U){
        dataPtr := 256.U
      }
      .elsewhen(counter === 14.U){
        dataPtr := (params.scratchpadEntries*8-1).U
        teststate := s_stop_wait // head and tail should move together
      }
      .elsewhen(counter === 18.U){
        dataPtr := (8-1).U
        teststate := s_stop_wait // head and tail should move together
      }
      .elsewhen(~memoryctrlIO.storeData.ready){
        teststate := s_write
      }

    }
    .elsewhen(teststate === s_stop_wait){
      when(memoryctrlIO.findMatchBegin){
        teststate := s_nomatch
      }
    }
    .elsewhen(teststate === s_write){
      when(memoryctrlIO.storeData.ready){
        teststate := s_match
        candidatePtr := 42.U
        dataPtr := 71.U
      }
    }
    .elsewhen(teststate === s_match){
      matchCounter := matchCounter + 1.U
      candidatePtr := candidatePtr + 1.U
      dataPtr := dataPtr + 1.U
      when(~equal){
        teststate := s_done
      }

    }

    scratchpadIO.write(1).en := (teststate === s_nomatch && counter < 128.U)
    scratchpadIO.write(1).addr := counter
    scratchpadIO.write(1).data := counter + 1.U
    memoryctrlIO.storeData.bits := counter + 1.U
    memoryctrlIO.storeData.valid := (teststate === s_nomatch && counter < 128.U)
    matchFound := (matchCounter === 0.U && teststate === s_match)
    equal := (matchCounter < 128.U && teststate === s_match)
  // ------------------------------------------------------------


  // initialize each operation
  when(cmd.fire()) {
    when(doSetLength) {
      length := cmd.bits.rs1
    }.elsewhen(doCompress) {
      minScratchpadAddress := cmd.bits.rs1
      maxScratchpadAddress := cmd.bits.rs1
      minWriteAddress := cmd.bits.rs2
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
  def makeDMARequest(write: Bool, virtualAddress: UInt, scratchpadAddress: UInt)(implicit p: Parameters, params: CompressionParameters): ScratchpadMemRequest = {
    val req = Wire(new ScratchpadMemRequest(params.scratchpadBanks, params.scratchpadEntries))
    req.vaddr := virtualAddress
    req.spbank := 0.U
    req.spaddr := scratchpadAddress
    req.write := write
    req
  }
}
