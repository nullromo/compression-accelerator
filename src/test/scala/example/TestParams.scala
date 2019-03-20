package example

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.system._

/**
  * Specify command-line type parameters for running the tests
  */
object TesterArgs {
  def apply() = {
    Array(
      "-fiwv",
      "--backend-name", "treadle",
      "--tr-write-vcd",
      "--target-dir", "results",
      "--top-name")
  }
}

/**
  * Get parameters for the accelerator from the full system.
  */
object AcceleratorParams {
  def apply() = {
    // Elaborate the full system first to get the tile parameters needed for RoCC
    // e.g. SharedMemoryTLEdge and TileKey
    val tileParams = LazyModule(new ExampleRocketSystem()(new CompressionAcceleratorConfig)).rocketTiles.head.p
    tileParams
  }
}