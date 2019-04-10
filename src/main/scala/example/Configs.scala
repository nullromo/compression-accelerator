package example

import chisel3._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.subsystem.{WithNBigCores, WithNMemoryChannels, WithRV32, WithRoccExample,RocketTilesKey}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.devices.tilelink.BootROMParams
import freechips.rocketchip.rocket.HasRocketCoreParameters
import freechips.rocketchip.tile.{BuildRoCC, HasCoreParameters, OpcodeSet, XLen}
import testchipip._

class WithBootROM extends Config((site, here, up) => {
  case BootROMParams => BootROMParams(
    contentFileName = s"./bootrom/bootrom.rv${site(XLen)}.img")
})

object ConfigValName {
  implicit val valName = ValName("TestHarness")
}
import ConfigValName._

class WithExampleTop extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters) => {
    Module(LazyModule(new ExampleTop()(p)).module)
  }
})

class WithPWM extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters) =>
    Module(LazyModule(new ExampleTopWithPWMTL()(p)).module)
})

class WithPWMAXI4 extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters) =>
    Module(LazyModule(new ExampleTopWithPWMAXI4()(p)).module)
})

class WithBlockDeviceModel extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters) => {
    val top = Module(LazyModule(new ExampleTopWithBlockDevice()(p)).module)
    top.connectBlockDeviceModel()
    top
  }
})

class WithSimBlockDevice extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters) => {
    val top = Module(LazyModule(new ExampleTopWithBlockDevice()(p)).module)
    top.connectSimBlockDevice(clock, reset)
    top
  }
})

class BaseExampleConfig extends Config(
  new WithBootROM ++
  new freechips.rocketchip.system.DefaultConfig)

class DefaultExampleConfig extends Config(
  new WithExampleTop ++ new BaseExampleConfig)

class RoccExampleConfig extends Config(
  new WithRoccExample ++ new DefaultExampleConfig)

class PWMConfig extends Config(new WithPWM ++ new BaseExampleConfig)

class PWMAXI4Config extends Config(new WithPWMAXI4 ++ new BaseExampleConfig)

class SimBlockDeviceConfig extends Config(
  new WithBlockDevice ++ new WithSimBlockDevice ++ new BaseExampleConfig)

class BlockDeviceModelConfig extends Config(
  new WithBlockDevice ++ new WithBlockDeviceModel ++ new BaseExampleConfig)

class WithTwoTrackers extends WithNBlockDeviceTrackers(2)
class WithFourTrackers extends WithNBlockDeviceTrackers(4)

class WithTwoMemChannels extends WithNMemoryChannels(2)
class WithFourMemChannels extends WithNMemoryChannels(4)

class DualCoreConfig extends Config(
  // Core gets tacked onto existing list
  new WithNBigCores(2) ++ new DefaultExampleConfig)

class RV32ExampleConfig extends Config(
  new WithRV32 ++ new DefaultExampleConfig)

class WithCompressionAccelerator extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => LazyModule(
      new CompressionAccelerator(OpcodeSet.custom3)(p)
    )
  )
  /*case RocketTilesKey => up(RocketTilesKey, site).copy(
	core = RocketCoreParams(nPMPs = false))*/
})

class CompressionAcceleratorConfig extends Config(
  new WithCompressionAccelerator ++ new DefaultExampleConfig
)
