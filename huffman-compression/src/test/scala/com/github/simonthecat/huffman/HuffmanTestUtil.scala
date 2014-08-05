package com.github.simonthecat.huffman

trait HuffmanTestUtil {
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
}
