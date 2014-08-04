package com.github.simonthecat.huffman

import org.specs2.mutable._

class HuffmanCompressorSpec extends Specification {
  /**
   * Simple function that creates freq tree from word, following rule:
   * subsequent chars are assigned frequency equal to their index
   * @param str reference string
   * @return freq tree
   */
  def simpleWordToFreqTree(str: String): FrequencyTree = {
    val charList = str.toCharArray.toSet.toList // so we're sure no duplicates exist
    val charsWithIntIndices =charList.zipWithIndex
    val charsWithNaiveFrequencies = charsWithIntIndices.map {
      case (char, intValue) => (char, intValue.toDouble)
    }
    FrequencyTree(charsWithNaiveFrequencies)
  }

  "Default HuffmanCompressor" should {
    "fail for letter not covered by freq tree" in {
      val tree = simpleWordToFreqTree("ab")
      val huffman = HuffmanCompressor(tree)

      huffman.compress("c") must throwAn[IllegalArgumentException]
    }

    "compress single letter covered by freq tree" in {
      val tree = simpleWordToFreqTree("ab")
      val huffman = HuffmanCompressor(tree)

      huffman.compress("a") === "L"
      huffman.compress("b") === "R"
    }

    "compress multiple letters covered by freq tree" in {
      val tree = simpleWordToFreqTree("ab")
      val huffman = HuffmanCompressor(tree)

      huffman.compress("aaa") === "LLL"
      huffman.compress("bbb") === "RRR"
      huffman.compress("abab") === "LRLR"
    }
  }
}
