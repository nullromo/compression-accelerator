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
    val testNum = 3
    val vecLength = 70
    val candidateVec = Array.ofDim[List[Int]](testNum)
    val dataVec = Array.ofDim[Int](testNum)
    val rangen = new Random(15)
    val maxOneByte = pow(2,8).toInt
    val maxTwoByte = pow(2,16).toInt

    val maxOneByteOffset = pow(2,11).toInt

    // test case 1:
    // - no buffer invalid problem
    // - parallel lane number is 1
    // - number of copy data is (copyByte) bytes
    // - test offset with 1 byte

    val copyByteTest1 = 4 + rangen.nextInt(8) 
    val sameDataTest1 = Array.fill[Int](copyByteTest1)(rangen.nextInt(maxOneByte)) // fill array with 1 byte integers
    val dummyCandidateTest1 = Array.fill[Int](vecLength-copyByteTest1)(1)
    val dummyDataTest1 = Array.fill[Int](vecLength-copyByteTest1)(2)

    candidateVec(0) = sameDataTest1 ++ dummyCandidateTest1
    dataVec(0) = sameDataTest1 ++ dummyDataTest1


    // test case 2:
    // - no buffer invalid problem
    // - parallel lane number is 1
    // - number of copy data is 32 bytes
    // - test offset with 2 bytes and 4 bytes

    val copyByteTest2 = 1 + rangen.nextInt(63)

    val sameDataTest2 = Array.fill[Int](copyByteTest2)(rangen.nextInt(maxOneByte)) // fill array with 1 byte integers
    val dummyCandidateTest2 = Array.fill[Int](vecLength-copyByteTest2)(1)
    val dummyDataTest2 = Array.fill[Int](vecLength-copyByteTest2)(2)

    candidateVec(1) = sameDataTest2 ++ dummyCandidateTest2
    candidateVec(2) = sameDataTest2 ++ dummyCandidateTest2

    dataVec(1) = sameDataTest2 ++ dummyDataTest1 
    dataVec(2) = sameDataTest2 ++ dummyDataTest2
    
    // generate 1/2/4 bytes offset between data and candidate
    val offsetVec = List(rangen.nextInt(maxOneByteOffset) rangen.nextInt(maxTwoByte), rangen.nextInt)

    // get golden result
    // Test1 result
    val testResult1: BigInt = offsetVec(0) & 0x0FF + offsetVec(0) & 0x700 << 5 + (copyByteTest1 - 4) << 10 + 1 << 8
    val testResult2: BigInt = offsetVec(1) & 0xFF00 >> 8 + offsetVec(1) & 0x00FF << 8 + copyByteTest2 << 18 + 2 << 16
    val testResult3: BigInt = offsetVec(2) & 0xFF000000 >> 24 + offsetVec(2) & 0x00FF0000 >> 8 + offsetVec(2) & 0x0000FF00 << 8 + offsetVec(2) & 0x000000FF << 24 + 3 << 32 + copyByteTest2 << 34

    val goldenRes = List(testResult1, testResult2, testResult3)

    it should "Test copy compression case 2" in{
        DoCopyCompressTester(params, candidateVec, dataVec, offsetVec, goldenRes) should be (true)
    }   


}