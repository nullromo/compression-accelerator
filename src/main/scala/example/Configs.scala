package example

import chisel3._
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.subsystem.{CanHaveMasterAXI4MemPort, CanHaveMasterAXI4MemPortModuleImp, HasRTCModuleImp, RocketTilesKey, WithNBigCores, WithNMemoryChannels, WithRV32, WithRoccExample}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.devices.tilelink.{BootROMParams, HasPeripheryBootROM, HasPeripheryBootROMModuleImp}
import freechips.rocketchip.rocket.{HasRocketCoreParameters, RocketCoreParams}
import freechips.rocketchip.system.{ExampleRocketSystem, ExampleRocketSystemModuleImp}
import freechips.rocketchip.tile.{BuildRoCC, HasCoreParameters, OpcodeSet, XLen}
import freechips.rocketchip.util.DontTouch
import testchipip._

class WithBootROM extends Config((site, here, up) => {
  case BootROMParams => BootROMParams(
    contentFileName = s"./bootrom/bootrom.rv${site(XLen)}.img")
})

object ConfigValName {
  implicit val valName = ValName("TestHarness")
}
import ConfigValName._

class ExampleTop(implicit p: Parameters) extends ExampleRocketSystem //RocketSubsystem
    with CanHaveMasterAXI4MemPort
    with HasPeripheryBootROM
    //  with HasSystemErrorSlave
    //    with HasSyncExtInterrupts
    with HasNoDebug
    with HasPeripherySerial {
  override lazy val module = new ExampleTopModule(this)
}

class ExampleTopModule[+L <: ExampleTop](l: L) extends ExampleRocketSystemModuleImp(l) // RocketSubsystemModuleImp(l)
    with HasRTCModuleImp
    with CanHaveMasterAXI4MemPortModuleImp
    with HasPeripheryBootROMModuleImp
    //    with HasExtInterruptsModuleImp
    with HasNoDebugModuleImp
    with HasPeripherySerialModuleImp
    with DontTouch

class WithExampleTop extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters) => {
    Module(LazyModule(new ExampleTop()(p)).module)
  }
})

class BaseExampleConfig extends Config(
  new WithBootROM ++
  new freechips.rocketchip.system.DefaultConfig)

class DefaultExampleConfig extends Config(
  new WithExampleTop ++ new BaseExampleConfig)

class RoccExampleConfig extends Config(
  new WithRoccExample ++ new DefaultExampleConfig)

class RV32ExampleConfig extends Config(
  new WithRV32 ++ new DefaultExampleConfig)

class WithCompressionAccelerator extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => LazyModule(
      new CompressionAccelerator(OpcodeSet.custom3)(p)
    )
  )
  case RocketTilesKey => up(RocketTilesKey, site).map(_.copy(
	core = RocketCoreParams(nPMPs = 0)))
})

class CompressionAcceleratorConfig extends Config(
  new WithCompressionAccelerator ++ new DefaultExampleConfig
)
