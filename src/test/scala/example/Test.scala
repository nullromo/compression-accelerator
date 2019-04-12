package example

import chisel3.{Driver => _, _}
import chisel3.iotesters._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tile.OpcodeSet
import org.scalatest.{FlatSpec, Matchers}
import treadle.TreadleTester
import example.TreadleTesterMemFunctions._
import scala.util.Random
import scala.math._
import java.io.File
import java.io.PrintWriter


class CompressionAcceleratorTester(c: ScratchpadTestModule) extends PeekPokeTester(c) {
	val randgen = new Random(15)
	val memory_data = Array.fill[Seq[Int]](8)(Array.fill[Int]((pow(2,10)-1).toInt)(randgen.nextInt(256)))

	for(i <- 0 until 8){
		val fileName = "memdata/memdata.hex_" + i + ".txt"
		val writer = new PrintWriter(new File(fileName))
		for(k <- 0 until memory_data(i).length){
			writer.write(memory_data(i)(k).toHexString)
			if(k != memory_data(i).length-1)
				writer.write("\n")
		}
		writer.close() 
		print("finsih 1 file write!\n")
		//val mem = "ram.mem_" + i
		//loadMemFromFile(fileName, mem)
	}
	//val mem = "ram.mem_" + i
	//loadMemFromFile(fileName, mem)
	//val mem_array = Array.ofDim[String](8)
	/*mem_array.zipWithIndex.foreach{case(mem, k) => mem_array(k) = "ram.mem_" + k
												   val fileName = "memdata/memdata_"+k+".txt"
												   loadMemFromFile(fileName, mem_array(k))}*/
  
	// set length
    poke(c.io.cmd.bits.inst.funct, 2) // doSetLength
    poke(c.io.cmd.bits.rs1, 100) // length = 100
    poke(c.io.cmd.valid, true) // fire
    step(1)
    // compress
    poke(c.io.cmd.bits.inst.funct, 0) // doCompress
    poke(c.io.cmd.bits.rs1, 0x0000) // src = 0
    poke(c.io.cmd.bits.rs2, 0x2000) // dst = 100
    step(1)
    poke(c.io.cmd.valid, false)
    step(1000)
	//  expect(peek(c.io.interrupt) != 277, "I should have passed ;(")

}

class CompressionAcceleratorSpec extends ChiselFlatSpec {
  implicit val p: Parameters = AcceleratorParams()

  val dutGen: () => ScratchpadTestModule = () => LazyModule(new ScratchpadTest(OpcodeSet.custom3)).module
  "CompressionAccelerator" should "accept commands" in {
    Driver.execute(TesterArgs()/* :+ "CompressionAccelerator"*/, dutGen) {
      c => new CompressionAcceleratorTester(c)
    } should be(true)
  }
}

class TreadleTest extends FlatSpec with Matchers {
  implicit val p: Parameters = AcceleratorParams()

  "Something" should "do something" in {
    val s = chisel3.Driver.emit(() => LazyModule(new ScratchpadTest(OpcodeSet.custom3)).module)
    implicit val tester: TreadleTester = new TreadleTester(s)

    tester.engine.makeVCDLogger("results/treadlevcd.vcd", showUnderscored = true)

	val randgen = new Random(15)
	val memory_data = Array.fill[Seq[Int]](8)(Array.fill[Int]((pow(2,10)-1).toInt)(randgen.nextInt(256)))

	for(i <- 0 until 8){
		val fileName = "memdata/memdata_" + i + ".txt"
		val writer = new PrintWriter(new File(fileName))
		for(k <- 0 until memory_data(i).length){
			writer.write(memory_data(i)(k).toString)
			if(k != memory_data(i).length-1)
				writer.write("\n")
		}
		writer.close() 
		print("finsih 1 file write!\n")
		//val mem = "ram.mem_" + i
		//loadMemFromFile(fileName, mem)
	}

	val mem_array = Array.ofDim[String](8)
	mem_array.zipWithIndex.foreach{case(mem, k) => mem_array(k) = "ram.mem_" + k
												   val fileName = "memdata/memdata_"+k+".txt"
												   loadMemFromFile(fileName, mem_array(k))}

    // set up memory
   	// val mem = "ram.mem_0"
    // loadMemFromFile("memdata/memdata_0.txt", mem)

    // start compression
    tester.poke("io_cmd_bits_inst_funct", 2)
    tester.poke("io_cmd_bits_rs1", 100)
    tester.poke("io_cmd_valid", 1)
    tester.step()
    tester.poke("io_cmd_bits_inst_funct", 0)
    tester.poke("io_cmd_bits_rs1", 0)
    tester.poke("io_cmd_bits_rs2", 100)
    tester.step()
    tester.poke("io_cmd_valid", 0)

    //do testing
    var dump: Seq[BigInt] = Seq()

    for(i <- 0 until 500) {
      tester.step()
      val newDump = read128Mem(mem_array(0))
      if(dump != newDump) {
        dump = newDump
        println("Cycle " + i)
        dump128Mem(mem_array(0))
      }
    }

    tester.engine.writeVCD()
  }
}
