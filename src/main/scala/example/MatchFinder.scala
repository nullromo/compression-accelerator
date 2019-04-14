package example

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.experimental.loadMemoryFromFile

class MatchFoundOutput(addressWidth: Int) extends Bundle {
  val matchA = Output(UInt(addressWidth.W))
  val matchB = Output(UInt(addressWidth.W))

  override def cloneType: this.type = new MatchFoundOutput(addressWidth).asInstanceOf[this.type]
}

class MatchFinderIO(dataWidth: Int, addressWidth: Int) extends Bundle {
  //ready/valid IO for producing the output
  val out = Decoupled(new MatchFoundOutput(addressWidth))
  // ready/valid IO for accepting the starting arguments
  val start = Flipped(Decoupled(UInt(addressWidth.W)))
  // continuous IO for accessing the scratchpad
  val newCandidateData = Input(UInt(dataWidth.W))
  val memoryReadAddress = Output(UInt(addressWidth.W))
  // global base pointer connection
  val globalBase = Input(UInt(addressWidth.W))

  override def cloneType: this.type = new MatchFinderIO(dataWidth, addressWidth).asInstanceOf[this.type]
}

class MatchFinder(dataWidth: Int, addressWidth: Int, hashTableSize: Int) extends Module {
  val io = IO(new MatchFinderIO(dataWidth, addressWidth))

  // true when looking for a match
  val looking = RegInit(false.B)

  // pointers given at the start of the search
  val matchPointer = RegInit(0.U(addressWidth.W))

  //TODO: add skip and bbhl for optimization
  //TODO: add update of the previous thing in the hash table after a copy is emitted for optimization

  // ready to start when we are not currently looking
  io.start.ready := !looking

  // when start looking, save the base pointer and set the state to looking
  when(io.start.fire() && !looking) {
    looking := true.B
    matchPointer := io.start.bits
  }

  // we want to read the scratchpad at the matchPointer location
  io.memoryReadAddress := matchPointer

  // create the hash table
  val hashTable = Module(new HashTable(32, 16, hashTableSize))

  // update the hash table with the new data and the offset of the new data
  hashTable.io.newData := io.newCandidateData
  hashTable.io.newOffset := matchPointer - io.globalBase

  // true when a match has been found
  val matchFound: Bool = Wire(Bool())
  matchFound := hashTable.io.oldData === io.newCandidateData

  // only write to the hash table when we are looking or starting to look
  hashTable.io.enable := looking || io.start.fire()

  // advance the searching pointer while looking for a match
  when(looking && !matchFound) {
    matchPointer := matchPointer + 1.U
  }

  // we have valid output when we are still in the looking state and there is a match
  io.out.valid := looking && matchFound

  // when the output is accepted, we are done with the looking state and ready to start again
  when(io.out.fire() && looking) {
    looking := false.B
  }

  // the beginning of the found match is the old offset for this data (the last location that held the same data)
  io.out.bits.matchA := hashTable.io.oldOffset + io.globalBase

  // the end of the found match is the place we are looking at
  io.out.bits.matchB := matchPointer
}

class MatchFinderTestModule(dataWidth: Int, addressWidth: Int, hashTableSize: Int) extends Module {
  // use the same IO as the matchFinder
  val io = IO(new MatchFinderIO(dataWidth, addressWidth))

  // create a matchFinder
  val matchFinder = Module(new MatchFinder(dataWidth: Int, addressWidth: Int, hashTableSize: Int))

  // create a memory
  val mem = Mem(4096, UInt(dataWidth.W))
  loadMemoryFromFile(mem, "memdata/memdata_1.txt")

  // connect the matchFinder to the memory
  matchFinder.io.newCandidateData := mem.read(matchFinder.io.memoryReadAddress)

  // pass the rest of the IO through
  matchFinder.io.start <> io.start
  io.out <> matchFinder.io.out
  io.memoryReadAddress := DontCare
  matchFinder.io.globalBase := io.globalBase
}