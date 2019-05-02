package example

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.system._
import treadle.TreadleTester

import scala.io.Source

/**
  * Specify command-line type parameters for running the tests.
  */
object TesterArgs {
  def apply(): Array[String] = {
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
  def apply(): Parameters = {
    // Elaborate the full system first to get the tile parameters needed for RoCC
    // e.g. SharedMemoryTLEdge and TileKey
    val tileParams = LazyModule(new ExampleRocketSystem()(new CompressionAcceleratorConfig)).rocketTiles.head.p
    tileParams
  }
}

object TreadleTesterMemFunctions {
  // returns the first 128 bytes of a memory
  def readMem(mem: Array[String], length: Int, banks: Int)(implicit tester: TreadleTester): List[BigInt] = {
    var arr: List[BigInt] = List()
    for(i <- 0 until length/banks) {
      for(bank <- 0 until banks) {
        arr = arr :+ tester.peekMemory(mem(bank), i)
      }
    }
    arr
  }

  // prints the first 128 bytes of a memory
  def dumpMem(data: List[BigInt])(implicit tester: TreadleTester): Unit = {
    for(i <- data.indices) {
      print(if(i%8==0) "\n" else " ")
      print("%2x".format(data(i)))
    }
    println("")
  }

  // loads data into a memory from a file
  def loadMemFromFile(filename: String, mem: String)(implicit tester: TreadleTester): Unit = {
    for((element, index) <- Source.fromFile(filename).getLines().zipWithIndex) {
      tester.pokeMemory(mem, index, Integer.parseInt(element, 16))
    }
  }
}
