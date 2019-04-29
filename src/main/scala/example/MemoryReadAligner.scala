package example

import chisel3._
import chisel3.core.dontTouch
import chisel3.util.experimental.loadMemoryFromFile     
import chisel3.util.{Cat, Decoupled, log2Ceil}

class DecoupledMemoryAlignerIO(addressWidth: Int, dataWidth: Int) extends Bundle {
  val address = Flipped(Decoupled(UInt(addressWidth.W)))
  val data = Decoupled(UInt(dataWidth.W))

  override def cloneType: this.type = new DecoupledMemoryAlignerIO(addressWidth, dataWidth).asInstanceOf[this.type]
}

class MemoryAlignerIO(addressWidth: Int, dataWidth: Int) extends Bundle {
  val address = Input(UInt(addressWidth.W))
  val en = Input(Bool())
  val data = Output(UInt(dataWidth.W))

  override def cloneType: this.type = new MemoryAlignerIO(addressWidth, dataWidth).asInstanceOf[this.type]
}

/**
  * Couples large-width memory with reads that are byte-addressable and want to see a smaller data size.
  */
class MemoryReadAligner(readAddressWidth: Int, readDataWidth: Int, memAddressWidth: Int, memDataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val readDataIO = new DecoupledMemoryAlignerIO(readAddressWidth, readDataWidth)
    val memDataIO = Flipped(new MemoryAlignerIO(memAddressWidth, memDataWidth))
    val readCandidateIO = new DecoupledMemoryAlignerIO(readAddressWidth, readDataWidth)
    val memCandidateIO = Flipped(new MemoryAlignerIO(memAddressWidth, memDataWidth))
    val equal = Input(Bool())
  })

  // special case for beginning
  val initializedData = RegInit(false.B)
  val initializedCandidate = RegInit(false.B)
  val initializedData_prev = RegInit(false.B)
  val initializedCandidate_prev = RegInit(false.B)

  // hold and tag the last read so that boundary-crossing reads can use it
  val cachedData = RegInit(VecInit(Seq.fill(2)(0.U(memDataWidth.W))))//RegInit(0.U(memDataWidth.W))
  val cachedCandidate = RegInit(VecInit(Seq.fill(2)(0.U(memDataWidth.W))))
  val cachedDataAddress = RegInit(0.U(memAddressWidth.W))
  val cachedCandidateAddress = RegInit(0.U(memAddressWidth.W))

  // bytes per read
  val memBytes: Int = memDataWidth / 8
  val readBytes: Int = readDataWidth / 8

  // Lower and upper read address for data and candidat in the buffer
  val lowerReadAddress_data = RegInit(0.U(readAddressWidth.W))
  val upperReadAddress_data = RegInit(0.U(readAddressWidth.W))
  val lowerReadOffset_data = Wire(UInt((log2Ceil(memBytes)+1).W))
  val bytesInLowerRead_data = Wire(UInt((log2Ceil(memBytes)).W))
  val bytesInUpperRead_data = Wire(UInt((log2Ceil(memBytes)).W))

  val lowerReadAddress_candidate = RegInit(0.U(readAddressWidth.W))
  val upperReadAddress_candidate = RegInit(0.U(readAddressWidth.W))
  val lowerReadOffset_candidate = Wire(UInt((log2Ceil(memBytes)+1).W))
  val bytesInLowerRead_candidate = Wire(UInt((log2Ceil(memBytes)).W))
  val bytesInUpperRead_candidate = Wire(UInt((log2Ceil(memBytes)).W))

  // reverse input from memory
  //val memDataRev = Wire(Vec(memBytes, UInt(8.W)))

  // Due to syncMemRead, one more cycle delayed
  val memRespData = RegInit(false.B)
  val memRespCandidate = RegInit(false.B)

  // Flag to indicate whether it's time to shift the register
  val shiftData = Wire(Bool())
  val shiftCandidate = Wire(Bool())
  val shiftData_prev = Reg(Bool())
  val shiftCandidate_prev = Reg(Bool())

  // Flag to indicate both cached data are fetched
  val readyData = RegInit(VecInit(Seq.fill(2)(false.B)))
  val readyCandidate = RegInit(VecInit(Seq.fill(2)(false.B)))

  // initial prev and initial
  initializedData_prev := initializedData
  initializedCandidate_prev := initializedCandidate

  // When the request data is in the upper data register, then shift
  shiftData := io.readDataIO.address.bits >= upperReadAddress_data*memBytes.U
  shiftCandidate := io.readCandidateIO.address.bits >= upperReadAddress_candidate*memBytes.U
  shiftData_prev := shiftData
  shiftCandidate_prev := shiftCandidate

  // Read SyncMem one cycel delay implementation
  memRespData := io.memDataIO.en
  memRespCandidate := io.memCandidateIO.en

  // do some math
  lowerReadOffset_data := Mux(~shiftData, io.readDataIO.address.bits % memBytes.U, io.readDataIO.address.bits % memBytes.U + memBytes.U)
  bytesInLowerRead_data  := Mux(lowerReadOffset_data + readBytes.U < memBytes.U,
    readBytes.U,
    memBytes.U - lowerReadOffset_data
  )
  bytesInUpperRead_data := readBytes.U - bytesInLowerRead_data

  lowerReadOffset_candidate := Mux(~shiftCandidate, io.readCandidateIO.address.bits % memBytes.U, io.readCandidateIO.address.bits % memBytes.U + memBytes.U)
  bytesInLowerRead_candidate := Mux(lowerReadOffset_candidate + readBytes.U < memBytes.U,
    readBytes.U,
    memBytes.U - lowerReadOffset_candidate
  )
  bytesInUpperRead_candidate := readBytes.U - bytesInLowerRead_candidate

  io.memDataIO.address := Mux(~initializedData || (initializedData && ~initializedData_prev), 
                              Mux(~memRespData, io.readDataIO.address.bits / memBytes.U, upperReadAddress_data), 
                              upperReadAddress_data+1.U)
  io.memDataIO.en := Mux(~initializedData || (initializedData && ~initializedData_prev), io.readDataIO.address.valid || memRespData, shiftData)
  io.readDataIO.data.valid := (io.readDataIO.address.valid && readyData(0) && readyData(1)) && (io.readDataIO.address.bits >= lowerReadAddress_data * memBytes.U)
  io.readDataIO.address.ready := ~((io.readDataIO.address.valid && ~readyData(1)) || (io.readDataIO.address.valid && (io.readDataIO.address.bits < lowerReadAddress_data*memBytes.U)))


  io.memCandidateIO.address := Mux(~initializedCandidate || (initializedCandidate && ~initializedCandidate_prev), 
                                    Mux(memRespCandidate, io.readCandidateIO.address.bits / memBytes.U, upperReadAddress_candidate), 
                                    upperReadAddress_candidate+1.U)
  io.memCandidateIO.en := Mux(~initializedCandidate || (initializedCandidate && ~initializedCandidate_prev), io.readCandidateIO.address.valid || memRespCandidate, shiftCandidate)
  io.readCandidateIO.data.valid := io.readCandidateIO.address.valid && readyCandidate(0) && readyCandidate(1)
  io.readCandidateIO.address.ready := ~(~initializedCandidate && readyCandidate(0) && ~readyCandidate(1))


  // Logic of data buffer
  // how does lower read address and upper read address changes

  when(io.readDataIO.address.valid){ // The first time fetch data
    when(~initializedData){
      lowerReadAddress_data := io.readDataIO.address.bits / memBytes.U
      upperReadAddress_data := io.readDataIO.address.bits / memBytes.U + 1.U
    }
    .otherwise{
      when(io.readDataIO.address.bits < lowerReadAddress_data*memBytes.U){
        initializedData := false.B
        readyData.foreach{(a) => a := false.B}
      }
      .elsewhen(shiftData){ // This is the time for cachedData to move and update
        lowerReadAddress_data := upperReadAddress_data
        upperReadAddress_data := upperReadAddress_data + 1.U
        cachedData(0) := cachedData(1)
      }
    }
  }

  // how does the data is stored
  when(~initializedData){
    when(memRespData){
      cachedData(0) := Cat(io.memDataIO.data.asTypeOf(Vec(memBytes, UInt(8.W)))) // don't know whether it will work or not
      readyData(0) := true.B
      initializedData := true.B
    }
  }
  .otherwise{
    when(memRespData && ~(io.readDataIO.address.bits < lowerReadAddress_data*memBytes.U)){
      cachedData(1) := Cat(io.memDataIO.data.asTypeOf(Vec(memBytes, UInt(8.W))))
      readyData(1) := true.B
    }
  }

  // data output
  // concatenate the appropriate bytes from the lower and upper read data into the final output
  val aggregateReadData: Vec[UInt] = Wire(Vec(memBytes * 2, UInt(8.W)))
  aggregateReadData := Mux(shiftData_prev && ~shiftData,
                           Cat(/*cachedData(0),*/ Cat(io.memDataIO.data.asTypeOf(Vec(memBytes, UInt(8.W)))), cachedData(0)).asTypeOf(Vec(memBytes * 2, UInt(8.W))),
                           Cat(/*cachedData(0),*/ cachedData(1), cachedData(0)).asTypeOf(Vec(memBytes * 2, UInt(8.W))))
  // select the correct output
  io.readDataIO.data.bits := Cat(
    (0 until readBytes).map({
      i => aggregateReadData(/*(memBytes * 2).U - */lowerReadOffset_data +& i.U)
    })
  )

  // ---------------- Logic of Candidate buffer -----------------------------------
  // how does lower read address and upper read address changes

  when(io.readCandidateIO.address.valid){ // The first time fetch data
    when(~initializedCandidate){
      lowerReadAddress_candidate := io.readCandidateIO.address.bits / memBytes.U
      upperReadAddress_candidate := io.readCandidateIO.address.bits / memBytes.U + 1.U
    }
    .otherwise{
      when(shiftCandidate){ // This is the time for cachedData to move and update
        lowerReadAddress_candidate := upperReadAddress_candidate
        upperReadAddress_candidate := upperReadAddress_candidate + 1.U
        cachedCandidate(0) := cachedCandidate(1)
      }
    }
  }

  // how does the Candidates is stored
  when(~initializedCandidate){
    when(memRespCandidate){
      cachedCandidate(0) := Cat(io.memCandidateIO.data.asTypeOf(Vec(memBytes, UInt(8.W)))) // don't know whether it will work or not
      readyCandidate(0) := true.B
      initializedCandidate := true.B
    }
  }
  .otherwise{
    when(memRespCandidate && io.equal){
      cachedCandidate(1) := Cat(io.memCandidateIO.data.asTypeOf(Vec(memBytes, UInt(8.W))))
      readyCandidate(1) := true.B
    }
  }

  // data output
  // concatenate the appropriate bytes from the lower and upper read data into the final output
  val aggregateReadCandidate: Vec[UInt] = Wire(Vec(memBytes * 2, UInt(8.W)))
  aggregateReadCandidate := Mux(shiftCandidate_prev && ~shiftCandidate,
                                Cat(/*cachedCandidate(0),*/ Cat(io.memCandidateIO.data.asTypeOf(Vec(memBytes, UInt(8.W)))), cachedCandidate(0)).asTypeOf(Vec(memBytes * 2, UInt(8.W))),
                                Cat(/*cachedCandidate(0),*/ cachedCandidate(1), cachedCandidate(0)).asTypeOf(Vec(memBytes * 2, UInt(8.W))))
  // select the correct output
  io.readCandidateIO.data.bits := Cat(
    (0 until readBytes).map({
      i => aggregateReadCandidate(/*(memBytes * 2).U - */lowerReadOffset_candidate +& i.U)
    })
  )

  when(~io.equal){
    initializedCandidate := false.B
    readyCandidate.foreach{(a) => a := false.B}
  }

}

class MemoryReadAlignerTestModule(readAddressWidth: Int, readDataWidth: Int, memAddressWidth: Int, memDataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val readIO = new DecoupledMemoryAlignerIO(readAddressWidth, readDataWidth)
  })

  // create aligner and backing mem
  val aligner = Module(new MemoryReadAligner(readAddressWidth, readDataWidth, memAddressWidth, memDataWidth))
  dontTouch(aligner.io)
  val backingMem = SyncReadMem(Math.pow(2, memAddressWidth).toInt, UInt(memDataWidth.W))
  loadMemoryFromFile(backingMem, "data/alignerTestData.txt")

  // connect the memory to the aligner
  aligner.io.memDataIO.data := backingMem.read(aligner.io.memDataIO.address, aligner.io.memDataIO.en)

  // connect the aligner to the io
  aligner.io.readDataIO <> io.readIO

  // initialize everything
  aligner.io.readCandidateIO.address.bits := 0.U
  aligner.io.readCandidateIO.address.valid := false.B
  aligner.io.readCandidateIO.data.ready := true.B
  aligner.io.memCandidateIO.data := 0.U
  aligner.io.equal := false.B
}
