package com.github.simonthecat.huffman

import org.specs2.mutable._

class HuffmanEncoderSpec extends Specification {
  /**
   * Simple function that creates freq tree from word, following rule:
   * subsequent chars are assigned frequency equal to their index
   * @param str reference string
   * @return freq tree
   */
  def simpleWordToFreqTree(str: String): FrequencyTree = {
    val charList = str.toCharArray.toSet.toList // so we're sure no duplicates exist
    val charsWithIntIndices = charList.zipWithIndex
    val charsWithNaiveFrequencies = charsWithIntIndices.map {
      case (char, intValue) => (char, intValue.toDouble)
    }
    FrequencyTree(charsWithNaiveFrequencies)
  }

  "HuffmanEncoder" should {
    "fail for letter not covered by freq tree" in {
      val tree = simpleWordToFreqTree("ab")
      val huffman = HuffmanEncoder(tree)

      huffman.encode("c") must throwAn[IllegalArgumentException]
    }

    "encode single letter covered by freq tree" in {
      val tree = simpleWordToFreqTree("ab")
      val huffman = HuffmanEncoder(tree)

      huffman.encode("a") === "L"
      huffman.encode("b") === "R"
    }

    "encode multiple letters covered by freq tree" in {
      val tree = simpleWordToFreqTree("ab")
      val huffman = HuffmanEncoder(tree)

      huffman.encode("aaa") === "LLL"
      huffman.encode("bbb") === "RRR"
      huffman.encode("abab") === "LRLR"
    }

    "work for custom output transformer" in {
      val zeroOneListTransformer: (Seq[Decision]) => List[Int] = { decisions =>
        decisions.map {
          case Left => 0
          case Right => 1
        }.toList
      }
      val tree = simpleWordToFreqTree("ab")
      val huffman = HuffmanEncoder(tree, zeroOneListTransformer)

      huffman.encode("abba") === List(0, 1, 1, 0)
    }
  }
}
