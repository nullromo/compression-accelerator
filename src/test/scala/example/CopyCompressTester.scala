import chisel3.iotesters._
import chisel3._
import chisel3.util._

class CopyCompressTester(c: CopyCompress, params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec: Seq[Int], goldenRes: Seq[BigInt]) extends PeekPokeTester(c){


}

object DoCopyCompressTester {
    def apply(params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec:Seq[Int], goldenRes: Seq[BigInt]): Boolean = {
        chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new CopyCompress(params)) {
            c => new CopyCompressTester(c, params, candidateVec, dataVec, offsetVec)
        }
    }
}