package example

import chisel3._
import chisel3.util._
import scala.math._

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
    val bufferPtrInc = Decoupled(UInt(log2Ceil(params.parallellane).W))
    val remain = Input(UInt(64.W)) // remaining byte number needs to be compressed

    override def cloneType: this.type = CopyCompressIO(params).asInstanceOf[this.type]
}
object CopyCompressIO{
    def apply (params: CopyCompressParams) : CopyCompressIO = 
        new CopyCompressIO(params)
}

class CopyCompress (val params: CopyCompressParams) extends Module{
    require(params.parallellane > 0)

    val io = IO(CopyCompressIO(params))

    val lengthAccum = RegInit(0.U(6.W)) // the maximum encoded length is 64 bytes
    val start_reg = RegInit(false.B)
    val num_candidate_valid = Wire(UInt((log2Ceil(params.parallellane)+1).U))
    val num_data_valid = Wire(UInt((log2Ceil(params.parallellane)+1).U))

    val compareResult = Wire(Vec(params.parallellane, Bool()))

    val reachEnd = Wire(Bool())

    val maxLength:Int = pow(2,6).toInt


    when(hit){
        start_reg := true.B
        lengthAccum := 0.U
    }
    when(~equal) start_reg := false.B

    num_candidate_valid := io.candidate.valid.reduce(_+_)
    num_data_valid := io.data.valid.reduce(_+_)

    // ******** Do comparison ***************
    for(i <- 0 until params.parallellane){
        when(io.data(i).valid === true.B && io.candidate(i).valid === true.B){
            when(io.data(i).bits === io.candidate(i).bits){
                compareResult(i) := true.B
            }
            .otherwise{
                compareResult(i) := false.B
            }
        }
        .otherwise{
          compareResult(i) := false.B
        }
    }

    // flag indicating whether the compression reaches the end
    reachEnd := (num_data_valid === 0.U) && (io.remain === 0.U)
    
    when(start_reg & io.copyStreamFormer.start.ready){

        io.bufferPtrInc.bits := 0.U // default value
        io.bufferPtrInc.valid := false.B

        // calculating how many same bytes between input data and candidate buffer
        // Because candidate should always in front of data, if # candidate < # data, candidate buffer is empty and needs to be fetched
        // But fetch data should always have priority
        // However, if # data is 0 and remain is 0, the compression is reach the end
        when(num_candidate_valid >= num_data_valid && ~reachEnd){
            for(i <- 0 until params.parallellane){
                when(compareResult.asUInt >= (pow(2,i+1).toInt - 1).U ){
                    when(lengthAccum < maxLength && ((lengthAccum + i.U) <= maxLength)){ // 6-bits represents 1-64 same bytes not 0-63
                        io.bufferPtrInc.bits := (i+1).U
                    }
                }
            }
            when(lengthAccum === 0.U){
                lengthAccum := lengthAccum + io.bufferPtrInc.bits -1.U
            }
            .otherwise{
                lengthAccum := lengthAccum + io.bufferPtrInc.bits
            }
            io.bufferPtrInc.valid := true.B
            when(io.bufferPtrInc.bits === num_data_valid){
                io.equal := true.B
            }
            .otherwise{
                io.equal := false.B
            }
        }
        .elsewhen(num_candidate_valid < num_data_valid && ~reachEnd){
            io.equal := true.B // remain same status
        }
        .otherwise{
            io.equal := false.B
        }
    }
    .elsewhen(start_reg){
        io.equal := true.B
        io.bufferPtrInc.bits := 0.U // default value
        io.bufferPtrInc.valid := true.B
    }
    .otherwise{
        io.equal := false.B
        io.bufferPtrInc.bits := 0.U // default value
        io.bufferPtrInc.valid := false.B
    }

    io.data.forall{(a) => a.ready := true.B}
    io.candidate.forall{(a) => a.ready := true.B}

    val copyStreamFormer = Module(new CopyStreamFormer(params))

    copyStreamFormer.io.offset.bits := io.offset.bits
    copyStreamFormer.io.offset.valid := io.offset.valid
    io.offset.ready := copyStreamFormer.io.offset.ready

    copyStreamFormer.io.lengthAccum := lengthAccum

    copyStreamFormer.io.start.bits := start_reg & (~io.equal)
    copyStreamFormer.io.start.valid := true.B

    copyStreamFormer.io.copyCompressed_one.bits := io.copyCompressed_one.bits
    copyStreamFormer.io.copyCompressed_two.bits := io.copyCompressed_two.bits
    copyStreamFormer.io.copyCompressed_four.bits := io.copyCompressed_four.bits
    copyStreamFormer.io.copyCompressed_one.valid := io.copyCompressed_one.valid
    copyStreamFormer.io.copyCompressed_two.valid := io.copyCompressed_two.valid
    copyStreamFormer.io.copyCompressed_four.valid := io.copyCompressed_four.valid

    io.copyCompressed_one.ready := copyStreamFormer.io.copyCompressed_one.ready
    io.copyCompressed_two.ready := copyStreamFormer.io.copyCompressed_two.ready
    io.copyCompressed_four.ready := copyStreamFormer.io.copyCompressed_four.ready

}


class CopyStreamFormerIO (params: CopyCompressParams) extends Bundle{

    val offset = Flipped(Decoupled(UInt(32.W))) // the maximum offset cannot be larger than 32bit integer
    val lengthAccum = Input(UInt(6.W))

    val copyCompressed_one = Decoupled(UInt(16.W)) // 1-byte offset copy
    val copyCompressed_two = Decoupled(UInt(24.W)) // 2-byte offset copy
    val copyCompressed_four = Decoupled(UInt(40.W)) // 4-byte offset copy

    val start = Flipped(Decoupled(Bool())) // tell the processor the stream is not a copy anymore (may due to mismatch or exceed max copy length)
    //val bufferPtrInc = Flipped(Decoupled(UInt(log2Ceil(params.parallellane).W)))

    override def cloneType: this.type = CopyStreamFormerIO(params).asInstanceOf[this.type]
}
object CopyStreamFormerIO{
    def apply (params: CopyCompressParams) : CopyStreamFormerIO = 
        new CopyStreamFormerIO(params)
}

class CopyStreamFormer (params: CopyCompressParams) extends Module{

    // Maximum latency should be 2 cycles

    val io = IO(CopyStreamFormerIO(params))

    val start_reg = RegInit(false.B)
    val length = RegInit(0.U(6.W))
    val copyOffset = RegInit(0.U(32.W))

    when(io.start.bits){
        start_reg := true.B
        length := io.lengthAccum
        copyOffset := io.offset.bits
    }

    when(start_reg){

        //default value
        io.copyCompressed_one.valid := false.B
        io.copyCompressed_two.valid := false.B
        io.copyCompressed_four.valid := false.B
        io.copyCompressed_one.bits := 0.U
        io.copyCompressed_two.bits := 0.U
        io.copyCompressed_four.bits := 0.U

        // hope it is little endian format
        when((copyOffset < (pow(2,11).toInt).U) && ((length+1.U) >= 4.U) && ((length+1.U) <= 11.U)){
            io.copyCompressed_one.valid := true.B
            io.copyCompressed_one.bits := (copyOffset & (0xFF).U) + (1.U << 8) + ((length - 4.U) << 10) + ((copyOffset & (0x700).U) << 5)
        }
        .elsewhen(copyOffset < (pow(2,16).toInt).U){
            io.copyCompressed_two.valid := true.B
            io.copyCompressed_two.bits := ((copyOffset & (0xFF00).U) >> 8) + ((copyOffset & (0x00FF).U) << 8) + (2.U << 16) + length << 18
        }
        .otherwise{
            io.copyCompressed_four.valid := true.B
            io.copyCompressed_four.bits := ((copyOffset & (0xFF000000).U) >> 24) + ((copyOffset & (0x00FF0000).U) >> 8) + ((copyOffset & (0x0000FF00).U) << 8) + ((copyOffset & (0x000000FF).U) << 24) + (3.U << 32) + length << 34
        }

        when(io.copyCompressed_one.ready){
            start_reg := false.B
        }
    }
    .otherwise{
        io.copyCompressed_one.valid := false.B
        io.copyCompressed_two.valid := false.B
        io.copyCompressed_four.valid := false.B
        io.copyCompressed_one.bits := 0.U
        io.copyCompressed_two.bits := 0.U
        io.copyCompressed_four.bits := 0.U
    }

    io.start.ready := ~start_reg
    io.offset.ready := true.B
}
