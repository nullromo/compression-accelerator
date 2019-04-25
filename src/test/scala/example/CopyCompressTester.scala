package example

import chisel3.iotesters._
import chisel3._
import chisel3.util._

class CopyCompressTester(c: CopyCompress, params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec: Seq[BigInt], goldenRes: Seq[BigInt]) extends PeekPokeTester(c){

    val maxWaitCycle = 50
    val numberTest: Int = candidateVec.length
    var currentParse = 0
    val inc: Int = params.parallellane
    var waitCounter = 0

    for(i <- 0 until numberTest){
        currentParse = 0

        poke(c.io.hit, true)
        poke(c.io.remain, 100)
        step(1)
        poke(c.io.hit, false)
        poke(c.io.copyCompressed.ready, 1)

        while( (currentParse == 0) || (peek(c.io.equal) == BigInt(1) && (currentParse+inc <= candidateVec(0).length))){
            c.io.candidate.zipWithIndex.foreach{case(in, idx) => poke(in.bits, candidateVec(i)(currentParse+idx))
                                                                 poke(in.valid, 1)}
            c.io.data.zipWithIndex.foreach{case(in, idx) => poke(in.bits, dataVec(i)(currentParse+idx))
                                                            poke(in.valid, 1)}
            poke(c.io.offset.bits, offsetVec(i))
            poke(c.io.offset.valid, 1)

            currentParse += inc

            step(1)
        }

        waitCounter = 0
        while(peek(c.io.copyCompressed.valid) == 0 && waitCounter <= maxWaitCycle){
            waitCounter += 1
            if (waitCounter >= maxWaitCycle){
                expect(good = false, "waited for output too long")
            }
            step(1)
        }
        
        if(peek(c.io.copyCompressed.valid) == BigInt(1)){
            println("CopyCompressed, tag = " + peek(c.io.copyCompressed.bits.tag))
            print("%x".format(peek(c.io.copyCompressed.bits.copy)))
            print("\n")
            print("%x".format(goldenRes(i)))
            print("\n")
            expect(c.io.copyCompressed.bits.copy, goldenRes(i))
        }

        step(1)

    }
    step(10)
}

object DoCopyCompressTesterLane1 {
    def apply(params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec: Seq[BigInt], goldenRes: Seq[BigInt]): Boolean = {
        chisel3.iotesters.Driver.execute(TesterArgs() :+ "CopyCompress1Lane", () => new CopyCompress(params)) {
            c => new CopyCompressTester(c, params, candidateVec, dataVec, offsetVec, goldenRes)
        }
    }
}

object DoCopyCompressTesterLane2 {
    def apply(params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec: Seq[BigInt], goldenRes: Seq[BigInt]): Boolean = {
        chisel3.iotesters.Driver.execute(TesterArgs() :+ "CopyCompress2Lane", () => new CopyCompress(params)) {
            c => new CopyCompressTester(c, params, candidateVec, dataVec, offsetVec, goldenRes)
        }
    }
}

object DoCopyCompressTesterLane4 {
    def apply(params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec: Seq[BigInt], goldenRes: Seq[BigInt]): Boolean = {
        chisel3.iotesters.Driver.execute(TesterArgs() :+ "CopyCompress4Lane", () => new CopyCompress(params)) {
            c => new CopyCompressTester(c, params, candidateVec, dataVec, offsetVec, goldenRes)
        }
    }
}
