package example

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.system._
import treadle.TreadleTester

import scala.io.Source

/**
  * Specify command-line type parameters for running the tests.
  */
object TesterArgs {
  def apply() = {
    Array(
	  "-tbn",
	  "verilator",
      "-fiwv",
      //"--backend-name", "treadle",
      //"--tr-write-vcd",
      //"--target-dir", "results",
      /*"--top-name"*/)
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

object TreadleTesterMemFunctions {
  // returns the first 128 bytes of a memory
  def read128Mem(mem: String)(implicit tester: TreadleTester): Seq[BigInt] = {
    var arr: Seq[BigInt] = Seq()
    for(i <- 0 until 128) {
      arr = arr :+ tester.peekMemory(mem, i)
    }
    arr
  }

  // prints the first 128 bytes of a memory
  def dump128Mem(mem: String)(implicit tester: TreadleTester): Unit = {
    for(i <- 0 until 128) {
      print(if(i%8==0) "\n" else " ")
      print(tester.peekMemory(mem, i))
    }
    println("")
  }

  // loads data into a memory from a file
  def loadMemFromFile(filename: String, mem: String)(implicit tester: TreadleTester): Unit = {
    for((element, index) <- Source.fromFile(filename).getLines().zipWithIndex) {
      tester.pokeMemory(mem, index, element.toInt)
    }
  }
}
