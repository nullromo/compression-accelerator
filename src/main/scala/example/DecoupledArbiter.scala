package example

import chisel3.util.{Decoupled, ShiftRegister, log2Ceil}
import chisel3._

/**
  * Arbitrates reads from a single-read-port scratchpad
  */
class DecoupledArbiter(addressWidth: Int, dataWidth: Int, numPorts: Int) extends Module {
  val io = IO(new Bundle {
    val address = Flipped(Decoupled(Vec(numPorts, UInt(addressWidth.W))))
    val data = Decoupled(Vec(numPorts, UInt(dataWidth.W)))
    val memAddress = Output(UInt(addressWidth.W))
    val memData = Input(UInt(dataWidth.W))
  })

  val addresses = Reg(Vec(numPorts, UInt(addressWidth.W)))

  val addressIndexMax: Int = numPorts
  val addressIndex = RegInit(addressIndexMax.U((log2Ceil(numPorts) + 1).W))
  val addressIndexWire = WireInit(addressIndexMax.U((log2Ceil(numPorts) + 1).W))
  addressIndex := addressIndexWire

  // only allow data to be valid for 1 cycle
  val wasValid = RegInit(false.B)
  wasValid := Mux(io.data.valid, true.B, wasValid)

  // on new input, reset the counter and accept new addresses, otherwise increment the counter
  when(io.address.fire()) {
    addressIndexWire := 0.U
    addresses := io.address.bits
    wasValid := false.B
  }.elsewhen(addressIndex < addressIndexMax.U) {
    addressIndexWire := addressIndex + 1.U
  }

  // interact with underlying mem
  for(i <- 0 until numPorts) {
    io.data.bits(i) := ShiftRegister(io.memData, numPorts - i - 1)
  }
  io.memAddress := addresses(addressIndex)

  // valid output and ready for more when we are done counting up
  io.address.ready := addressIndex === addressIndexMax.U
  io.data.valid := !wasValid && addressIndex === addressIndexMax.U
}

/**
  * Example of how to hook up the arbiter to an underlying memory
  */
class ArbiterWithMem(addressWidth: Int, dataWidth: Int, numPorts: Int) extends Module {
  val io = IO(new Bundle {
    val address = Flipped(Decoupled(Vec(numPorts, UInt(addressWidth.W))))
    val data = Decoupled(Vec(numPorts, UInt(dataWidth.W)))
  })

  val arbiter = Module(new DecoupledArbiter(addressWidth, dataWidth, numPorts))
  val mem = Module(BasicMem(MemParameters(256, 32, syncRead = true, bypass = false)))

  mem.io.writeEnable := false.B
  mem.io.writeAddress := DontCare
  mem.io.writeData := DontCare

  mem.io.readAddress := arbiter.io.memAddress
  arbiter.io.memData := mem.io.readData

  io.address <> arbiter.io.address
  io.data <> arbiter.io.data
}