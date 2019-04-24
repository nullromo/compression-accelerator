package example

import chisel3._
import chisel3.core.dontTouch
import chisel3.util.Decoupled
import chisel3.util.experimental.loadMemoryFromFile

class MatchFinderIO(dataWidth: Int, addressWidth: Int) extends Bundle {
    // ready/valid IO for accepting the starting arguments
    //TODO: start probably doesn't have to be decoupled
    val start = Flipped(Decoupled(Bool()))
    // continuous IO for the matchB pointer
    val matchB = Input(UInt(addressWidth.W))
    // ready/valid IO for scratchpad data
    val newData = Flipped(Decoupled(UInt(dataWidth.W)))
    // global base pointer connection
    val src = Input(UInt(addressWidth.W))
    // reset the hash table
    val clear = Input(Bool())
    //ready/valid IO for producing the output
    val matchA = Decoupled(UInt(addressWidth.W))

    override def cloneType: this.type = new MatchFinderIO(dataWidth, addressWidth).asInstanceOf[this.type]
}

class MatchFinder(dataWidth: Int, addressWidth: Int, hashTableSize: Int) extends Module {
    val io = IO(new MatchFinderIO(dataWidth, addressWidth))

    // true when looking for a match
    val looking = RegInit(false.B)

    // when start looking, save the base pointer and set the state to looking
    when(io.start.fire()) {
        looking := true.B
    }

    // when the output is accepted, we are done with the looking state and ready to start again
    when(io.matchA.fire()) {
        looking := false.B
    }

    //TODO: add skip and bbhl for optimization
    //TODO: add update of the previous thing in the hash table after a copy is emitted for optimization

    // ready to start when we are not currently looking
    io.start.ready := !looking

    // always ready to receive data from the memory
    io.newData.ready := true.B

    // create the hash table
    val hashTable = Module(new HashTable(32, 16, hashTableSize))

    // clear the present bits in the hash table when a brand new compression job is starting
    hashTable.io.clearPresent := io.clear

    // update the hash table with the new data and the offset of the new data
    hashTable.io.newData := io.newData.bits
    hashTable.io.newOffset := io.matchB - io.src

    // true when a match has been found
    val matchFound: Bool = Wire(Bool())

    matchFound := hashTable.io.oldPresent &&
        (hashTable.io.oldData === io.newData.bits) &&
        io.newData.valid &&
        io.matchA.bits =/= io.matchB

    // only write to the hash table when we are looking or starting to look
    hashTable.io.enable := io.newData.valid && (looking || io.start.fire())

    // we have valid output when we are still in the looking state and there is a match
    io.matchA.valid := looking && matchFound

    // the beginning of the found match is the old offset for this data (the last location that held the same data)
    io.matchA.bits := hashTable.io.oldOffset + io.src
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
    //TODO: this shouldn't depend on the memory aligner... it doesn't matter that much though as long as things work
    val aligner = Module(new MemoryReadAligner(
        32, 32, 32, 64
    ))

    // connect aligner to memory
    aligner.io.memIO.data := mem.read(aligner.io.memIO.address)

    // connect the matchFinder to the read aligner
    aligner.io.readIO.address.bits := io.matchB
    aligner.io.readIO.address.valid := true.B
    matchFinder.io.newData <> aligner.io.readIO.data

    // pass the rest of the IO through
    matchFinder.io.start <> io.start
    io.matchA <> matchFinder.io.matchA
    io.newData <> DontCare
    matchFinder.io.src := io.src
    matchFinder.io.matchB := io.matchB

    dontTouch(aligner.io)
    dontTouch(matchFinder.io)
}