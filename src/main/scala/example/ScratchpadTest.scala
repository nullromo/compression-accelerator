package example

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.devices.tilelink.TLTestRAM
import freechips.rocketchip.tile.{LazyRoCC, LazyRoCCModuleImp, OpcodeSet}

class ScratchpadTest(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new ScratchpadTestModule(this)
}

class ScratchpadTestModule(outer: ScratchpadTest)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {
  val ram = LazyModule(new TLTestRAM(AddressSet(0, 0xFFFF), beatBytes = 8))
  val accelerator = LazyModule(new CompressionAccelerator(outer.opcodes))

  ram.node := accelerator.tlNode
}