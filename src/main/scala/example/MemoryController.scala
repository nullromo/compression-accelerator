package example

import chisel3._
import chisel3.core.dontTouch
import chisel3.util._
import chisel3.util.log2Ceil
import external.{Scratchpad, ScratchpadMemIO}
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
    val offset = Input(UInt(coreMaxAddrBits.W))             // the matched data(candidate) address offset
    val matchFound = Input(Bool())                          // whether a match is found
    val equal = Input(Bool())                               // whether the data stream is still in copy mode (from copy datapath)

    // -- Input from store queue
    val storeData = Flipped(Decoupled(UInt(8*dataBytes.W))) // compressed data stream

    // -- Output
    val readScratchpadReady = Output(Bool())                // whether scratchpad is ready to be read
    val writeScratchpadReady = Output(Bool())               // whether scratchpad is ready to be written
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
        io = IO(new MemoryControllerIO(nRows, dataBytes))
    }

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

    val (s_idle :: s_fill :: s_working :: Nil) = Enum(3)
    val stateWork = RegInit(s_idle)

    // full logic
    fullLD := ~(headLDp === tailLDp)
    fullSW := ~(headSWp === tailSWp)

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
            when(io.dma.resp.error){
                printf("DMA returned read error=true in a response (page fault?)\n")
            }
            .otherwise{
                when(emptyLD){
                    emptyLD = false.B
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
        //            special case should be handled in matching data path by comparing the candidate address with io.minLDvAdrr

        // case 1: when a match is found
        //       -- move the head of 
        when(io.matchFound){

        }
    }


} 

object DMAReq {
  def makeDMARequest(write: Bool, spbank: UInt, virtualAddress: UInt, scratchpadAddress: UInt)(implicit p: Parameters, params: CompressionParameters): ScratchpadMemRequest = {
    val req = Wire(new ScratchpadMemRequest(params.scratchpadBanks, params.scratchpadEntries))
    req.vaddr := virtualAddress
    req.spbank := 0.U
    req.spaddr := scratchpadAddress
    req.write := write
    req
  }
}