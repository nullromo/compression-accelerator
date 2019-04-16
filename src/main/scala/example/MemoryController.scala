package example

import chisel3._
import chisel3.core.dontTouch
import chisel3.util._
import chisel3.util.log2Ceil
import external.{Scratchpad, ScratchpadMemIO}
import freechips.rocketchip.tile._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket._

class MemoryControllerIO(val nRows: Int, val dataBytes: Int)(implicit p: Parameters) extends CoreBundle {
    // --Inputs
    val readBaseAddr = Input(UInt(coreMaxAddrBits.W))       // to be compressed Data base address
    val writeBaseAddr = Input(UInt(coreMaxAddrBits.W))      // compressed data save base address
    val length = Input(UInt(32.W))                          // Total data length to be compressed
    val busy = Input(Bool())                                // whether the compression begins
    val dataPtr = Flipped(Decoupled(UInt(log2Ceil(nRows*dataBytes).W)))      // the next data byte that needs to be compressed (should be scratchpad address, not virtual address)
    val candidatePtr = Flipped(Decoupled(UInt(log2Ceil(nRows*dataBytes).W))) // candidate pointer
    //val offset = Input(UInt(coreMaxAddrBits.W))             // the matched data(candidate) address offset
    val matchFound = Input(Bool())                          // whether a match is found
    val equal = Input(Bool())                               // whether the data stream is still in copy mode (from copy datapath)
    val endEncode = Input(Bool())                           // all encoding is finished

    // -- Input from store queue
    val storeData = Flipped(Decoupled(UInt((8*dataBytes).W))) // compressed data stream

    // -- Output
    val readScratchpadReady = Output(Bool())                // whether scratchpad is ready to be read
    //val writeScratchpadReady = Output(Bool())               // whether scratchpad is ready to be written
    val findMatchBegin = Output(Bool())                     // whether datapath can run matching
    val minvAddr = Output(UInt(coreMaxAddrBits.W))          // the minimum data (load) virtual address in the scratchpad
    val maxvAddr = Output(UInt(coreMaxAddrBits.W))          // the maximum data (load) virtual address in the scratchpad

    // -- DMA arbiter port to Scratchpad
    val dma = new ScratchpadMemIO(2, nRows)                 // 2 banks: 0 -> load bank   1 -> store bank

}

class MemoryController(val nRows: Int, val w: Int, val dataBits: Int = 64)(implicit p: Parameters) extends LazyModule {
    val dataBytes = dataBits / 8

    // Real implementation
    lazy val module = new LazyModuleImp(this) with HasCoreParameters {
        val io = IO(new MemoryControllerIO(nRows, dataBytes))
    

        // load head/tail ptrs
        val headLDp = RegInit(0.U(log2Ceil(nRows).W))
        val tailLDp = RegInit(1.U(log2Ceil(nRows).W))
        val fullLD = Wire(Bool())
        val emptyLD = RegInit(true.B)

        // store head/tail ptrs
        val headSWp = RegInit(0.U(log2Ceil(nRows).W))
        val tailSWp = RegInit(1.U(log2Ceil(nRows).W))
        val fullSW = Wire(Bool())
        val emptySW = RegInit(true.B)

        // load/store address tracker
        val minLDvAddr = RegInit(0.U(coreMaxAddrBits.W))
        val maxLDvAddr = RegInit(0.U(coreMaxAddrBits.W))
        val minSWvAddr = RegInit(0.U(coreMaxAddrBits.W))
        val maxSWvAddr = RegInit(0.U(coreMaxAddrBits.W))

        val (s_idle :: s_fill :: s_working :: s_write :: s_done ::
            s_dma_wait :: s_dma_read :: s_dma_write :: Nil) = Enum(8)
        val stateWork = RegInit(s_idle) // determine head and tail 
        val stateDMA = RegInit(s_idle)  // determine dma read and write

        val endLoad = Wire(Bool())
        val outOfRange = Wire(Bool())

        endLoad := (maxLDvAddr >= (io.readBaseAddr + io.length))
        outOfRange := (io.dataPtr.bits === (tailLDp * dataBytes.U - 1.U))

        // min virtual address
        minvAddr := minLDvAddr
        maxvAddr := maxLDvAddr

        // full logic
        fullLD := ~(headLDp === tailLDp)
        fullSW := ~(headSWp === tailSWp)

        // store compressed data into scratchpad
        // -- because each dma store needs to store all data in store scratchpad, tail should not move during write tp L2$
        when(io.storeData.fire()){
            tailSWp := tailSWp + 1.U
        }
        io.storeData.ready := ~(fullSW || (stateDMA === s_dma_write))

        // dma request logic
        // -- DMA will send request when stateDMA is at s_dma_read or s_dma_write
        // -- When loading data, virtual address should be maxLDvAddr, spaddress should be tail ptr, and bank is 0
        // -- When writing data, virtual address should be maxSWvAddr, spaddress should be tail ptr, and bank is 1
        io.dma.req.bits.vaddr := Mux(stateDMA === s_dma_read, maxLDvAddr, maxSWvAddr)
        io.dma.req.bits.spaddr := Mux(stateDMA === s_dma_read, tailLDp, tailSWp)
        io.dma.req.bits.spbank := Mux(stateDMA === s_dma_read, 0.U, 1.U)
        io.dma.req.bits.write := stateDMA === s_dma_write
        io.dma.req.valid := ((stateDMA === s_dma_read) || (stateDMA === s_dma_write))
        io.dma.resp.ready := true.B

        // connect the rest of the output
        io.readScratchpadReady := ~emptyLD && (stateWork > s_fill) && ~outOfRange
        io.findMatchBegin := (~(outOfRange || (stateDMA === s_dma_write))) && (stateWork > s_fill)

        when(stateWork === s_idle){
            when(io.busy){
                minLDvAddr := io.readBaseAddr
                maxLDvAddr := io.readBaseAddr
                minSWvAddr := io.writeBaseAddr
                maxSWvAddr := io.writeBaseAddr
                stateWork := s_fill
            }
        }
        .elsewhen(stateWork === s_fill){
            when(io.dma.resp.valid){
                when(io.dma.resp.bits.error){
                    printf("DMA returned filling read error=true in a response (page fault?)\n")
                }
                .otherwise{
                    when(emptyLD){
                        emptyLD := false.B
                    }
                    .otherwise{
                        tailLDp := tailLDp + 1.U
                    }
                    maxLDvAddr := maxLDvAddr + dataBytes.U
                }
            }

            // when load scratchpad is full or data has all been fetched into scratchpad
            // state will switch to working state
            when(fullLD || (maxLDvAddr - minLDvAddr >= io.length)){
                stateWork := s_working
            }
        }
        .elsewhen(stateWork === s_working){
            // attention: In this controller, we assume that offset will always within nRows*dataBytes range
            //            special case should be handled in matching data path by comparing the candidate address with io.minLDvAddr

            // waiting for dma response
            when(io.dma.resp.valid){
                when(io.dma.resp.bits.error){
                    printf("DMA returned working read error=true in a response (page fault?)\n")
                }
                .otherwise{
                    tailLDp := tailLDp + 1.U
                    maxLDvAddr := maxLDvAddr + dataBytes.U
                }
                stateWork := s_working
            }

            // case 1: when a match is found
            //       -- move the head of LDp to candidate ptr line (assume that is the data is not consumed, in_ptr won't move)
            //       -- update minLDvAddr
            // case 4: when doing copy compression, head will change with candidatePtr
            when(io.matchFound || io.equal){
                headLDp := io.candidatePtr.bits >> log2Ceil(dataBytes) // get the line number
                minLDvAddr := minLDvAddr + Mux(headLDp <= (io.candidatePtr.bits >> log2Ceil(dataBytes)), ((io.candidatePtr.bits >> log2Ceil(dataBytes)) - headLDp)*dataBytes.U, ((io.candidatePtr.bits >> log2Ceil(dataBytes)) - headLDp + nRows.U)*dataBytes.U)
            }

            // case 2: when no match found but load scratchpad is full and dataPtr reaches the end of the scratchpad
            //        -- move head first and then tail together
            //        -- request DMA
            when((io.dataPtr.bits  === (tailLDp * dataBytes.U - 1.U)) && fullLD){
                headLDp := headLDp + 1.U
                minLDvAddr := minLDvAddr + dataBytes.U
            }

            // case 3: when store scratchpad is full or encode ends
            when((fullSW || io.endEncode) && io.dma.req.ready){
                stateWork := s_write
            }
        }
        .elsewhen(stateWork === s_write){
            when(io.dma.resp.valid){
                when(io.dma.resp.bits.error){
                    printf("DMA returned Out-of-range read error=true in a response (page fault?)\n")
                }
                .otherwise{
                    when((headSWp === tailSWp) && ~emptySW){
                        emptySW := true.B
                    }.otherwise{
                        headLDp := headLDp + 1.U
                    }
                    minSWvAddr := minSWvAddr + dataBytes.U
                }
            }

            when(io.dma.req.ready && emptySW){
                when(~io.endEncode){
                    stateWork := s_working
                }
                .otherwise{
                    stateWork := s_done
                }
            }
        }
        .elsewhen(stateWork === s_done){
            when(~io.busy){
                stateWork := s_idle
            }
        }

        // DMA state machine
        when(stateDMA === s_idle){
            when(io.busy){
                stateDMA := s_dma_wait
            }
        }
        .elsewhen(stateDMA === s_dma_wait){
            when(fullSW || io.endEncode){
                stateDMA := s_dma_write
            }
            .elsewhen((~fullLD || outOfRange) && ~endLoad){
                stateDMA := s_dma_read
            }

        }
        .elsewhen(stateDMA === s_dma_read){
            when(fullSW && io.dma.req.ready){
                stateDMA := s_dma_write
            }
            .elsewhen((fullLD || endLoad) && io.dma.req.ready){
                stateDMA := s_dma_wait
            }
        }
        .elsewhen(stateDMA === s_dma_write){
            when(emptySW && fullLD && ~io.endEncode && io.dma.req.ready){
                stateDMA := s_dma_wait
            }
            .elsewhen(emptySW && io.endEncode && io.dma.req.ready){
                stateDMA := s_done
            }
            .elsewhen(emptySW && ~fullLD && io.dma.req.ready){
                stateDMA := s_dma_read
            }
        }
        .elsewhen(stateDMA === s_done){
            when(~io.busy){
                stateDMA := s_idle
            }
        }

    }



} 