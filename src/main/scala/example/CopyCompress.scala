package example

import chisel3._
import chisel3.experimental.dontTouch
import chisel3.util._

import scala.math._

trait CopyCompressParams {
    val parallellane: Int
}

// if the tag is 1, then only the first 16 bits of copy are valid
// if the tag is 2, then only the first 24 bits of copy are valid
// if the tag is 3, then all 40            bits of copy are valid
class CopyCompressedBundle extends Bundle {
    val copy = Output(UInt(40.W))
    val tag = Output(UInt(2.W))
}

class CopyCompressIO(params: CopyCompressParams) extends Bundle {
    val candidate: Vec[DecoupledIO[UInt]] = Vec(params.parallellane, Flipped(Decoupled(UInt(8.W)))) // 1 byte with multiple of parallellane number
    val data: Vec[DecoupledIO[UInt]] = Vec(params.parallellane, Flipped(Decoupled(UInt(8.W)))) // 1 byte with multiple of parallelane number

    val offset = Flipped(Decoupled(UInt(32.W))) // the maximum offset cannot be larger than 32bit integer

    val copyCompressed = Decoupled(new CopyCompressedBundle)

    val equal = Output(Bool()) // tell the processor the stream is not a copy anymore (may due to mismatch or exceed max copy length)
    val hit = Input(Bool()) // tell the datapath to start compress
    val bufferPtrInc = Decoupled(UInt(log2Ceil(params.parallellane + 1).W))
    val remain = Input(UInt(64.W)) // remaining byte number needs to be compressed

    override def cloneType: this.type = CopyCompressIO(params).asInstanceOf[this.type]
}

object CopyCompressIO {
    def apply(params: CopyCompressParams): CopyCompressIO =
        new CopyCompressIO(params)
}

class CopyCompress(val params: CopyCompressParams) extends Module {
    require(params.parallellane > 0)

    val io = IO(CopyCompressIO(params))

    val lengthAccum = RegInit(0.U(6.W)) // the maximum encoded length is 64 bytes
    val start_reg = RegInit(false.B)
    val candidate_valid: Vec[UInt] = Wire(Vec(params.parallellane, UInt(1.W)))
    val data_valid: Vec[UInt] = Wire(Vec(params.parallellane, UInt(1.W)))
    val num_candidate_valid: UInt = Wire(UInt((log2Ceil(params.parallellane) + 1).W))
    val num_data_valid: UInt = Wire(UInt((log2Ceil(params.parallellane) + 1).W))

    val compareResult: Vec[Bool] = Wire(Vec(params.parallellane, Bool()))
    val compareResult_uint: UInt = Wire(UInt(params.parallellane.W))
    val equal_reg = Reg(Bool())
    val equal_reg_prev = Reg(Bool())
    dontTouch(compareResult)
    dontTouch(compareResult_uint)
    dontTouch(num_candidate_valid)
    dontTouch(num_data_valid)
    dontTouch(io.bufferPtrInc.bits)
    dontTouch(candidate_valid)
    dontTouch(data_valid)

    val reachEnd: Bool = Wire(Bool())

    val maxLength: Int = pow(2, 6).toInt

    val copyStreamFormer = Module(new CopyStreamFormer(params))

    compareResult_uint := compareResult.asUInt
    equal_reg := io.equal
    equal_reg_prev := equal_reg


    when(io.hit) {
        start_reg := true.B
        lengthAccum := 0.U
        equal_reg := true.B
        equal_reg_prev := true.B
    }
        .elsewhen(!io.equal) {
            start_reg := false.B
        }

    (io.candidate zip candidate_valid).foreach { case (a, b) => b := a.valid.asUInt }
    (io.data zip data_valid).foreach { case (a, b) => b := a.valid.asUInt }

    num_candidate_valid := candidate_valid.reduce(_ +& _)
    num_data_valid := data_valid.reduce(_ +& _)

    // ******** Do comparison ***************
    for (i <- 0 until params.parallellane) {
        when(io.data(i).valid === true.B && io.candidate(i).valid === true.B) {
            when(io.data(i).bits === io.candidate(i).bits) {
                compareResult(i) := true.B
            }
                .otherwise {
                    compareResult(i) := false.B
                }
        }
            .otherwise {
                compareResult(i) := false.B
            }
    }

    // flag indicating whether the compression reaches the end
    reachEnd := (num_data_valid === 0.U) && (io.remain === 0.U)

    when(start_reg & copyStreamFormer.io.start.ready) {

        io.bufferPtrInc.bits := 0.U // default value
        io.bufferPtrInc.valid := false.B

        // calculating how many same bytes between input data and candidate buffer
        // Because candidate should always in front of data, if # candidate < # data, candidate buffer is empty and needs to be fetched
        // But fetch data should always have priority
        // However, if # data is 0 and remain is 0, the compression is reach the end
        // It has problem here !!!!!!
        when(num_candidate_valid >= num_data_valid && !reachEnd) {
            for (i <- 0 until params.parallellane) {
                when((compareResult.asUInt & (pow(2, i + 1) - 1).toInt.U) === (pow(2, i + 1) - 1).toInt.U) {
                    when(lengthAccum < maxLength.U && ((lengthAccum + i.U) <= maxLength.U)) { // 6-bits represents 1-64 same bytes not 0-63
                        io.bufferPtrInc.bits := (i + 1).U
                    }
                }
            }

            lengthAccum := lengthAccum + io.bufferPtrInc.bits
            io.bufferPtrInc.valid := true.B
            when(io.bufferPtrInc.bits === num_data_valid) {
                io.equal := true.B
            }
                .otherwise {
                    io.equal := false.B
                }
        }
            .elsewhen(num_candidate_valid < num_data_valid && !reachEnd) {
                io.equal := equal_reg //true.B // remain same status
            }
            .otherwise {
                io.equal := false.B
            }
    }
        .elsewhen(start_reg) {
            io.equal := equal_reg //true.B
            io.bufferPtrInc.bits := 0.U // default value
            io.bufferPtrInc.valid := true.B
        }
        .otherwise {
            io.equal := false.B
            io.bufferPtrInc.bits := 0.U // default value
            io.bufferPtrInc.valid := false.B
        }

    io.data.foreach { a => a.ready := true.B }
    io.candidate.foreach { a => a.ready := true.B }


    copyStreamFormer.io.offset.bits := io.offset.bits
    copyStreamFormer.io.offset.valid := io.offset.valid
    io.offset.ready := copyStreamFormer.io.offset.ready

    copyStreamFormer.io.lengthAccum := lengthAccum

    copyStreamFormer.io.start.bits := equal_reg_prev && (!equal_reg) //start_reg & (!io.equal)
    copyStreamFormer.io.start.valid := true.B

    io.copyCompressed <> copyStreamFormer.io.copyCompressed

}


class CopyStreamFormerIO(params: CopyCompressParams) extends Bundle {

    val offset = Flipped(Decoupled(UInt(32.W))) // the maximum offset cannot be larger than 32bit integer
    val lengthAccum = Input(UInt(6.W))

    val copyCompressed = Decoupled(new CopyCompressedBundle)

    val start = Flipped(Decoupled(Bool())) // tell the processor the stream is not a copy anymore (may due to mismatch or exceed max copy length)
    //val bufferPtrInc = Flipped(Decoupled(UInt(log2Ceil(params.parallellane).W)))

    override def cloneType: this.type = CopyStreamFormerIO(params).asInstanceOf[this.type]
}

object CopyStreamFormerIO {
    def apply(params: CopyCompressParams): CopyStreamFormerIO =
        new CopyStreamFormerIO(params)
}

class CopyStreamFormer(params: CopyCompressParams) extends Module {

    // Maximum latency should be 2 cycles

    val io = IO(CopyStreamFormerIO(params))

    val start_reg = RegInit(false.B)
    val length = RegInit(0.U(6.W))
    val copyOffset = RegInit(0.U(32.W))

    when(io.start.bits) {
        start_reg := true.B
        length := io.lengthAccum
        copyOffset := io.offset.bits
    }

    when(start_reg) {

        //default value
        io.copyCompressed.valid := false.B
        io.copyCompressed.bits.copy := 0.U
        io.copyCompressed.bits.tag := 0.U

        // hope it is little endian format
        when((copyOffset < pow(2, 11).toInt.U) && (length >= 4.U) && (length <= 11.U)) {
            io.copyCompressed.bits.tag := 0x1.U
            io.copyCompressed.valid := true.B
            io.copyCompressed.bits.copy := (copyOffset & 0xFF.U) | (1.U << 8).asUInt() |
                ((length - 4.U) << 10).asUInt() |
                ((copyOffset & 0x700.U) << 5).asUInt()
        }
            .elsewhen(copyOffset < pow(2, 16).toInt.U) {
                io.copyCompressed.bits.tag := 0x2.U
                io.copyCompressed.valid := true.B
                io.copyCompressed.bits.copy := ((copyOffset & 0xFF00.U) >> 8).asUInt() |
                    ((copyOffset & 0x00FF.U) << 8).asUInt() |
                    (2.U << 16).asUInt() |
                    ((length - 1.U) << 18).asUInt()
            }
            .otherwise {
                io.copyCompressed.bits.tag := 0x3.U
                io.copyCompressed.valid := true.B
                io.copyCompressed.bits.copy := ((copyOffset & 0xFF000000L.U) >> 24).asUInt() |
                    ((copyOffset & 0x00FF0000L.U) >> 8).asUInt() |
                    ((copyOffset & 0x0000FF00L.U) << 8).asUInt() |
                    ((copyOffset & 0x000000FFL.U) << 24).asUInt() |
                    (3.U << 32).asUInt() |
                    ((length - 1.U) << 34).asUInt()
                printf("I am here, %x\n", copyOffset)
                printf("I am here again, %x\n", length)
                printf("I also am here %x\n", io.copyCompressed.bits.copy)
            }

        when(io.copyCompressed.ready) {
            start_reg := false.B
        }
    }
        .otherwise {
            io.copyCompressed.valid := false.B
            io.copyCompressed.bits.copy := 0.U
            io.copyCompressed.bits.tag := 0.U
        }

    io.start.ready := !start_reg
    io.offset.ready := true.B
}
