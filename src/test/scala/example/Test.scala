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

    // set length
    poke(c.io.cmd.bits.inst.funct, 2) // doSetLength
    poke(c.io.cmd.bits.rs1, 100) // length = 100
    poke(c.io.cmd.valid, true) // fire
    step(1)
    // compress
    poke(c.io.cmd.bits.inst.funct, 0) // doCompress
    poke(c.io.cmd.bits.rs1, 0x000) // src = 0
    poke(c.io.cmd.bits.rs2, 0x064) // dst = 100
    step(1)
    poke(c.io.cmd.valid, false)
    step(1000)
    //  expect(peek(c.io.interrupt) != 277, "I should have passed ;(")

}

class CompressionAcceleratorSpec extends ChiselFlatSpec {
    implicit val p: Parameters = AcceleratorParams()

    val dutGen: () => ScratchpadTestModule = () => LazyModule(new ScratchpadTest(OpcodeSet.custom3)).module
    "CompressionAccelerator" should "accept commands" in {
        Driver.execute(TesterArgs() :+ "CompressionAccelerator", dutGen) {
            c => new CompressionAcceleratorTester(c)
        } should be(true)
    }
}

class GenerateMemdata extends FlatSpec with Matchers {
    "Memdata generator" should "generate data" in {
        val randgen = new Random(15)
        val memory_data = Array.fill[Seq[Int]](8)(Array.fill[Int]((pow(2, 8) - 1).toInt)(randgen.nextInt(256)))

        for (i <- 0 until 8) {
            val fileName = "data/memdata.hex_" + i + ".txt"
            val writer = new PrintWriter(new File(fileName))
            for (k <- memory_data(i).indices) {
                writer.write(memory_data(i)(k).toHexString)
                if (k != memory_data(i).length - 1)
                    writer.write("\n")
            }
            writer.close()
            print("finsih 1 file write!\n")
            //val mem = "ram.mem_" + i
            //loadMemFromFile(fileName, mem)
        }

        val fileNameall = "" +
            "data/memdata.hex.txt"
        val writerall = new PrintWriter(new File(fileNameall))
        for (i <- memory_data(0).indices) {
            var storeData: String = ""
            for (k <- 0 until 8) {
                if (memory_data(k)(i) < 16)
                    storeData = storeData + "0" + memory_data(k)(i).toHexString
                else
                    storeData = storeData + memory_data(k)(i).toHexString
            }
            writerall.write(storeData)
            if (i != memory_data(0).length - 1)
                writerall.write("\n")
        }
        writerall.close()
        //val mem = "ram.mem_" + i
        //loadMemFromFile(fileName, mem)
        //val mem_array = Array.ofDim[String](8)
        /*mem_array.zipWithIndex.foreach{case(mem, k) => mem_array(k) = "ram.mem_" + k
                                                       val fileName = "data/memdata_"+k+".txt"
                                                       loadMemFromFile(fileName, mem_array(k))}*/

    }
}

class TreadleTest extends FlatSpec with Matchers {
    implicit val p: Parameters = AcceleratorParams()

    "Something" should "do something" in {
        val s = chisel3.Driver.emit(() => LazyModule(new ScratchpadTest(OpcodeSet.custom3)).module)
        implicit val tester: TreadleTester = new TreadleTester(s)

        tester.engine.makeVCDLogger("results/treadle-CompressionAccelerator.vcd", showUnderscored = true)

        //load initial memory
        val mem_array = Array.ofDim[String](8)
        mem_array.zipWithIndex.foreach { case (mem, k) => mem_array(k) = "ram.mem_" + k
            val fileName = "data/alignerTestData_" + k + ".txt"
            loadMemFromFile(fileName, mem_array(k))
        }
        tester.step(200)

        // start compression
        //set length: 100
        tester.poke("io_cmd_bits_inst_funct", 2)
        tester.poke("io_cmd_bits_rs1", 100)
        tester.poke("io_cmd_valid", 1)
        tester.step()
        //compress from address 0 into address 100
        tester.poke("io_cmd_bits_inst_funct", 0)
        tester.poke("io_cmd_bits_rs1", 0)
        tester.poke("io_cmd_bits_rs2", 100)
        tester.step()
        //stop sending commands
        tester.poke("io_cmd_valid", 0)

        //run for many cycles
        var dump: List[BigInt] = List()
        for (i <- 0 until 1600) {

            //advance
            tester.step()

            //test TLTestRAM memory for changes
            val newDump = readMem(mem_array, 200, 8)
            if (i % 200 == 0 && i != 0)
                println("Cycle " + i)
            if (dump != newDump) {
                dump = newDump
                println("Cycle " + i)
                dumpMem(dump)
            }

            //check dma req valid
            val spaddr = tester.peek("accelerator.memoryctrl.io_dma_req_bits_spaddr")
            val spbank = tester.peek("accelerator.memoryctrl.io_dma_req_bits_spbank")
            val vaddr = tester.peek("accelerator.memoryctrl.io_dma_req_bits_vaddr")
            val write = tester.peek("accelerator.memoryctrl.io_dma_req_bits_write")

            println("matchB: " + tester.peek("accelerator.matchB"))

            if (write != 0) {
                println("dma write. cycle " + i + " (" + write + ")")
            }
        }
        println()

        tester.engine.writeVCD()
    }
}
