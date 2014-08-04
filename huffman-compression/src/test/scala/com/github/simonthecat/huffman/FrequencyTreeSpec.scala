package com.github.simonthecat.huffman

import org.specs2.mutable._

object FrequencyTreeSpec extends Specification {
  "FrequencyTree" should {
    "fail for empty frequency input" in {
      val freqs: List[(Char, Double)] = Nil
      FrequencyTree(freqs) must throwAn[IllegalArgumentException]
    }

    "be build for single freq" in {
      val freqs = List(('a', 5.0))
      val tree = FrequencyTree(freqs)
      tree.letterSet === Set('a')
      tree.freq === 5.0
    }

    "be build for two asc ordered freqs" in {
      val freqs = List(('a', 3.0), ('b', 5.0))
      val tree = FrequencyTree(freqs)
      tree.letterSet === Set('a', 'b')
      tree.freq === 8.0
    }

    "be build for two desc ordered freqs" in {
      val freqs = List(('a', 10.0), ('b', 2.0))
      val tree = FrequencyTree(freqs)
      val bLeaf = Leaf(2.0, 'b')
      val aLeaf = Leaf(10.0, 'a')
      tree === Node(12.0, bLeaf, aLeaf, Set('b', 'a'))
    }

    "be build for two three freqs" in {
      val freqs = List(('a', 10.0), ('b', 3.0), ('c', 4.0))
      val tree = FrequencyTree(freqs)

      val aLeaf = Leaf(10.0, 'a')
      val bLeaf = Leaf(3.0, 'b')
      val cLeaf = Leaf(4.0, 'c')
      val bcTree = Node(7.0, bLeaf, cLeaf, Set('b', 'c'))
      val expectedTree = Node(17.0, bcTree, aLeaf, Set('b', 'c', 'a'))

      tree === expectedTree
    }
  }
}
