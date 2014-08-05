package com.github.simonthecat.huffman

import org.specs2.mutable._

class HuffmanDecoderSpec extends Specification with HuffmanTestUtil {
  "HuffmanDecoder" should {
    "decode empty input" in {
      val tree = simpleWordToFreqTree("abc")
      val decoder = HuffmanDecoder(tree)

      decoder.decode("") === "".toSeq
    }

    "decode simple input" in {
      val tree = simpleWordToFreqTree("abc")
      val decoder = HuffmanDecoder(tree)

      decoder.decode("LLLR") === "ab".toSeq
      decoder.decode("LRLL") === "ba".toSeq
      decoder.decode("RRR") === "ccc".toSeq
    }

    "decode more complex input" in {
      val tree = FrequencyTree(Seq(
        ('a', 1.0), ('b', 2.0), ('c', 5.0), ('d', 10.0), ('e', 30.0)
      ))
      val decoder = HuffmanDecoder(tree)

      decoder.decode("RRR") === "eee".toSeq
      decoder.decode("RRRLLLL") === "eeea".toSeq
    }

    "throw exception on invalid input" in {
      val tree = simpleWordToFreqTree("abc")
      val decoder = HuffmanDecoder(tree)

      decoder.decode("L") must throwAn[IllegalArgumentException]
    }
  }
}
