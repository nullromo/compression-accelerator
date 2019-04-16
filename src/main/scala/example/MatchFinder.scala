package example

import chisel3._
import chisel3.core.dontTouch
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
  // ready/valid IO for scratchpad address
  val memoryReadAddress = Decoupled(UInt(addressWidth.W))
  // ready/valid IO for scratchpad data
  val newCandidateData = Flipped(Decoupled(UInt(dataWidth.W)))
  // global base pointer connection
  val globalBase = Input(UInt(addressWidth.W))
  // reset the hash table
  val clear = Input(Bool())

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

  // always ready to receive data from the memory
  io.newCandidateData.ready := true.B

  // when start looking, save the base pointer and set the state to looking
  when(io.start.fire() && !looking) {
    looking := true.B
    matchPointer := io.start.bits
  }

  // we want to read the scratchpad at the matchPointer location
  io.memoryReadAddress.bits := matchPointer

  // the address sent to the memory is always valid
  io.memoryReadAddress.valid := true.B

  // create the hash table
  val hashTable = Module(new HashTable(32, 16, hashTableSize))

  // clear the present bits in the hash table when a brand new compression job is starting
  hashTable.io.clearPresent := io.clear

  // update the hash table with the new data and the offset of the new data
  hashTable.io.newData := io.newCandidateData.bits
  hashTable.io.newOffset := matchPointer - io.globalBase

  // true when a match has been found
  val matchFound: Bool = Wire(Bool())

  matchFound := hashTable.io.oldPresent &&
    (hashTable.io.oldData === io.newCandidateData.bits) &&
    io.newCandidateData.valid &&
    io.out.bits.matchA =/= io.out.bits.matchB

  // only write to the hash table when we are looking or starting to look
  hashTable.io.enable := io.newCandidateData.valid && (looking || io.start.fire())

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

class MatchFinderTestModule(memDataWidth: Int, dataWidth: Int, addressWidth: Int, hashTableSize: Int) extends Module {
  // use the same IO as the matchFinder
  val io = IO(new MatchFinderIO(dataWidth, addressWidth))

  // create a matchFinder
  val matchFinder = Module(new MatchFinder(dataWidth: Int, addressWidth: Int, hashTableSize: Int))

  // we don't need to clear the hash table
  matchFinder.io.clear := false.B

  // create a memory
  val mem = Mem(4096, UInt(memDataWidth.W))
  loadMemoryFromFile(mem, "data/alignerTestData.txt")

  // create memory aligner adapter
  //TODO: this shouldn't depend on the memory aligner... it doens't matter that much though as long as things work
  val aligner = Module(new MemoryReadAligner(
    32, 32, 32, 64
  ))

  // connect aligner to memory
  aligner.io.memIO.data := mem.read(aligner.io.memIO.address)

  // connect the matchFinder to the read aligner
  aligner.io.readIO.address <> matchFinder.io.memoryReadAddress
  matchFinder.io.newCandidateData <> aligner.io.readIO.data

  // pass the rest of the IO through
  matchFinder.io.start <> io.start
  io.out <> matchFinder.io.out
  io.memoryReadAddress <> DontCare
  io.newCandidateData <> DontCare
  matchFinder.io.globalBase := io.globalBase

  dontTouch(aligner.io)
  dontTouch(matchFinder.io)
}