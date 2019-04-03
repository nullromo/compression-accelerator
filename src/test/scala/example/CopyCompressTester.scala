package example

import chisel3.iotesters._
import chisel3._
import chisel3.util._

class CopyCompressTester(c: CopyCompress, params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec: Seq[BigInt], goldenRes: Seq[BigInt]) extends PeekPokeTester(c){

    val maxWaitCycle = 50
    val numberTest = candidateVec.length
    var currentParse = 0
    val inc = params.parallellane
    var waitCounter = 0

    for(i <- 0 until numberTest){
        currentParse = 0

        poke(c.io.hit, true)
        poke(c.io.remain, 100)
        step(1)
        poke(c.io.hit, false)
        poke(c.io.copyCompressed_one.ready, 1)
        poke(c.io.copyCompressed_two.ready, 1)
        poke(c.io.copyCompressed_four.ready, 1)

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
        while(!(peek(c.io.copyCompressed_one.valid) == BigInt(1) || peek(c.io.copyCompressed_two.valid) == BigInt(1) || peek(c.io.copyCompressed_four.valid) == BigInt(1)) && waitCounter <= maxWaitCycle){
            waitCounter += 1
            if (waitCounter >= maxWaitCycle){
                expect(false, "waited for output too long")
            }
            step(1)
        }
        
        if(peek(c.io.copyCompressed_one.valid) == BigInt(1)){
            println("CopyCompressed_one")
            print(peek(c.io.copyCompressed_one.bits))
            print("\n")
            print(goldenRes(i))
            print("\n")
            expect(c.io.copyCompressed_one.bits, goldenRes(i))
        }
        else if(peek(c.io.copyCompressed_two.valid) == BigInt(1)){
            expect(c.io.copyCompressed_two.bits, goldenRes(i))
            println("CopyCompressed_two")
            print(peek(c.io.copyCompressed_two.bits))
            print("\n")
            print(goldenRes(i))
            print("\n")
        }
        else if(peek(c.io.copyCompressed_four.valid) == BigInt(1)){
            expect(c.io.copyCompressed_four.bits, goldenRes(i))
            println("CopyCompressed_four")
            print(peek(c.io.copyCompressed_four.bits))
            print("\n")
            print(goldenRes(i))
            print("\n")
        }

        step(1)

    }
    step(10)
}

object DoCopyCompressTester {
    def apply(params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec: Seq[BigInt], goldenRes: Seq[BigInt]): Boolean = {
        chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new CopyCompress(params)) {
            c => new CopyCompressTester(c, params, candidateVec, dataVec, offsetVec, goldenRes)
        }
    }
}
