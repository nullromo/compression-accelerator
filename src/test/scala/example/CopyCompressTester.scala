import chisel3.iotesters._
import chisel3._
import chisel3.util._

class CopyCompressTester(c: CopyCompress, params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec: Seq[Int], goldenRes: Seq[BigInt]) extends PeekPokeTester(c){

    val maxWaitCycle = 50
    val numberTest = candidateVec.length
    var currentParse = 0
    val inc = params.parallellane
    var waitCounter = 0

    for(i <- 0 until numberTest){
        currentParse = 0

        poke(c.io.hit, true)
        step(1)
        poke(c.io.hit, false)
        poke(c.io.remain, 100)
        poke(c.io.copyCompressed_one.ready, 1)
        poke(c.io.copyCompressed_two.ready, 1)
        poke(c.io.copyCompressed_four.ready, 1)

        while( (currentParse == 0) || (peek(c.io.equal) && (currentParse+inc <= candidateVec(0).length))){
            c.io.candidate.bits.zipWithIndex.foreach{case(in, idx) => poke(in, candidateVec(i)(currentParse+idx))}
            c.io.data.bits.zipWithIndex.foreach{case(in, idx) => poke(in, dataVec(i)(currentParse+idx))}
            poke(c.io.offset.bits, offsetVec(i))

            poke(c.io.candidate.valid, 1)
            poke(c.io.data.valid, 1)
            poke(c.io.offset.valid, 1)

            currentParse += inc

            step(1)
        }

        waitCounter = 0
        while(~(peek(c.io.copyCompressed_one.valid) || peek(c.io.copyCompressed_two.valid) || peek(c.io.copyCompressed_four.valid)) && waitCounter < maxWaitCycle){
            waitCounter += 1
            if (waitCounter >= maxWaitCycle){
                expect(false, "waited for output too long")
            }
            step(1)
        }
        
        if(peek(c.io.copyCompressed_one.valid)){
            expect(c.io.copyCompressed_one.bits, goldenRes(i))
        }
        else if(peek(c.io.copyCompressed_two.valid)){
            expect(c.io.copyCompressed_two.bits, goldenRes(i))
        }
        else if(peek(c.io.copyCompressed_four.valid)){
            expect(c.io.copyCompressed_four.bits, goldenRes(i))
        }

        step(1)

    }
}

object DoCopyCompressTester {
    def apply(params: CopyCompressParams, candidateVec: Seq[Seq[Int]], dataVec: Seq[Seq[Int]], offsetVec:Seq[Int], goldenRes: Seq[BigInt]): Boolean = {
        chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new CopyCompress(params)) {
            c => new CopyCompressTester(c, params, candidateVec, dataVec, offsetVec)
        }
    }
}