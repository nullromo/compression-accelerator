package example

import chisel3._
import chisel3.core.dontTouch
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.tilelink.TLTestRAM
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.groundtest.DummyPTW
import freechips.rocketchip.tile.{OpcodeSet, RoCCCommand, RoCCResponse}
import freechips.rocketchip.tilelink.{TLBuffer, TLFragmenter, TLXbar}
import chisel3.util.experimental.loadMemoryFromFile

class ScratchpadTest(opcodes: OpcodeSet, val data: String = "")(implicit p: Parameters) extends LazyModule() {
    override lazy val module = new ScratchpadTestModule(this, data)
    val ram = LazyModule(new TLTestRAM(AddressSet(0, 0xFFFFFF), beatBytes = 8))
    val xbar = LazyModule(new TLXbar)
    val accelerator = LazyModule(new CompressionAccelerator(opcodes))
    xbar.node :=* accelerator.tlNode
    ram.node := TLFragmenter(8, 64) := TLBuffer() := xbar.node
}

class ScratchpadTestModule(outer: ScratchpadTest, data: String)(implicit p: Parameters) extends LazyModuleImp(outer) {
    val io = IO(new Bundle {
        val cmd = Flipped(Decoupled(new RoCCCommand))
        val resp = Decoupled(new RoCCResponse)
        val busy = Output(Bool())
        val interrupt = Output(Bool())
        val exception = Input(Bool())
        val busyCycles = Output(UInt(64.W))
    })

    outer.accelerator.module.io.cmd <> io.cmd
    io.resp <> outer.accelerator.module.io.resp
    io.busy := outer.accelerator.module.io.busy
    io.interrupt := outer.accelerator.module.io.interrupt
    outer.accelerator.module.io.exception := io.exception
    loadMemoryFromFile(outer.ram.module.mem, data)
//    loadMemoryFromFile(outer.ram.module.mem, "data/alignerTestData.txt")

    val ptw = Module(new DummyPTW(1))
    ptw.io.requestors <> outer.accelerator.module.io.ptw

    // count total cycles taken
    val busyCycles = RegInit(0.U(64.W))
    dontTouch(busyCycles)
    when(outer.accelerator.module.io.busy) {
        busyCycles := busyCycles + 1.U
    }
    io.busyCycles := busyCycles
}
