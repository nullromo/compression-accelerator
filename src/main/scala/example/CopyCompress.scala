package example

import chisel3._
import chisel3.util._

trait CopyCompressParams{
    val parallellane: Int
}

class CopyCompressIO (params: CopyCompressParams) extends Bundle{
    val candidate = Vec(params.parallellane, Flipped(Decoupled(UInt(8.W)))) // 1 byte with multiple of parallellane number
    val data = Vec(params.parallellane, Flipped(Decoupled(UInt(8.W)))) // 1 byte with multiple of parallelane number

    val offset = Flipped(Decoupled(UInt(32.W))) // the maximum offset cannot be larger than 32bit integer

    val copyCompressed_one = Decoupled(UInt(16.W)) // 1-byte offset copy
    val copyCompressed_two = Decoupled(UInt(24.W)) // 2-byte offset copy
    val copyCompressed_four = Decoupled(UInt(40.W)) // 4-byte offset copy

    val equal = Output(Bool()) // tell the processor the stream is not a copy anymore (may due to mismatch or exceed max copy length)
    val hit = Input(Bool()) // tell the datapath to start compress
    val idx = Decoupled(UInt(log2Ceil(params.parallellane)))

    override def cloneType: this.type = CopyCompressIO(params).asInstanceOf[this.type]
}
object CopyCompressIO{
    def apply (params: CopyCompressParams) : CopyCompressIO = 
        new CopyCompressIO(params)
}

object TreeReduce {
  def apply[V](in: Seq[V], func: (V, V) => V): V = {
    if (in.length == 1) {
      return in(0)
    }
    if (in.length == 2) {
      return func(in(0), in(1))
    }
    if (in.length % 2 == 0) {
      val withIdxs = in.zipWithIndex
      val evens = withIdxs.filter{case (_, idx) => idx % 2 == 0}.map(_._1)
      val odds  = withIdxs.filter{case (_, idx) => idx % 2 != 0}.map(_._1)
      val evenOddPairs: Seq[(V, V)] = evens zip odds
      return TreeReduce(evenOddPairs.map(x => func(x._1, x._2)), func)
    } else {
      return TreeReduce(Seq(in(0), TreeReduce(in.drop(1), func)), func)
    }
  }
}

class CopyCompress (val params: CopyCompressParams) extends Module{
    require(params.parallellane > 0)

    val io = IO(CopyCompressIO(params))

    val lengthAccum = RegInit(0.U(6.W)) // the maximum encoded length is 64 bytes


}

