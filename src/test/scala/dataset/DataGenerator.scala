package dataset

import java.io.{File, PrintWriter}

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.Random

/**
  * Generates test input data for compression benchmarking.
  */
class DataGenerator extends FlatSpec with Matchers {
    "DataGenerator" should "generate benchmark data" in {
        // seeded random number generator
        val rand = new Random(4444)

        // lengths of files to generate
        val lengths: Seq[Int] = Seq(10, 20, 50, 100, 200, 500, 1000, 2000)

        // output directory
        val outputDir = "benchmark-data/"

        // entire text of Fox in Socks
        val foxInSocksSource = Source.fromFile("data/fox-in-socks.txt")
        val foxInSocks = try foxInSocksSource.mkString finally foxInSocksSource.close()

        /**
          * Generates files of random, real and repeating data
          */
        def generateData(): Unit = {
            for (length <- lengths) {

                // make a file of random data
                val randomWriter = new PrintWriter(new File(outputDir + "random-" + length + ".dat"))

                // make a file from Fox in Socks
                val realWriter = new PrintWriter(new File(outputDir + "real-" + length + ".dat"))

                // make a file of all a's
                val repeatingWriter = new PrintWriter(new File(outputDir + "repeating-" + length + ".dat"))

                // write the data
                for (i <- 0 until length) {
                    randomWriter.write(rand.nextInt(256))
                    realWriter.write(foxInSocks(i))
                    repeatingWriter.write("a")
                }

                // close the files
                randomWriter.close()
                realWriter.close()
                repeatingWriter.close()
            }
        }

        // actually do it
        generateData()
    }
}
