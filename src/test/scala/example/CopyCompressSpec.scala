package example

import org.scalatest.{FlatSpec, Matchers}
import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class CopyCompressSpec extends FlatSpec with Matchers{
    behavior of "CopyCompress"

    val params = new CopyCompressParams{
        val parallellane = 1
    }

    // Because the maximum length is 64, then the test case vector will be 70
    val vecLength = 70
    val candidateVec = Array.ofDim[Int](vecLength)
    val dataVec = Array.ofDim[Int](vecLength)
    val rangen = new Random(15)
    val maxOneByte = pow(2,8).toInt
    val maxTwoByte = pow(2,16).toInt

    // test case 1:
    // - no buffer invalid problem
    // - parallel lane number is 1
    // - number of copy data is 32 bytes
    // - test offset with 1 byte, 2 bytes and 4 bytes

    val sameData = Array.fill[Int](32)(rangen.nextInt(maxOneByte)) // fill array with 1 byte integers
    (candidateVec.slice(0,32) zip sameData).foreach{case(cand, indata) => cand = indata}
    (dataVec.slice(0,32) zip sameData).foreach{case(data, indata) => data = indata}

    candidateVec.slice(32, vecLength).foreach(_ = rangen.nextInt(maxOneByte))
    dataVec.slice(32,vecLength).foreach(_ = rangen.nextInt(maxOneByte))

    // generate 1/2/4 bytes offset between data and candidate
    val offsetVec = List(rangen.nextInt(maxOneByte), rangen.nextInt(maxTwoByte), rangen.nextInt)

    it should "Test copy compression case 1" in{
        DoCopyCompressTester(params, candidateVec, dataVec, offsetVec) should be (true)
    }   


}