package com.github.simonthecat.huffman

import scala.annotation.tailrec

sealed trait FrequencyTree {
  val freq: Double

  def letterSet: Set[Char] = this match {
    case n: Node => n.letters
    case l: Leaf => Set(l.letter)
  }
}

case class Node(freq: Double, left: FrequencyTree, right: FrequencyTree, letters: Set[Char]) extends FrequencyTree

case class Leaf(freq: Double, letter: Char) extends FrequencyTree

object FrequencyTree {
  type FreqChar = (Char, Double)

  private def combine(a: FrequencyTree, b: FrequencyTree): FrequencyTree = {
    val letters = a.letterSet ++ b.letterSet
    val freq = a.freq + b.freq
    Node(freq, a, b, letters.toSet)
  }

  @tailrec
  private def buildTree(trees: List[FrequencyTree]): FrequencyTree = trees match {
    case x :: Nil => x
    case _ =>
      val sorted = trees.sorted
      val (toCombine, rest) = sorted.splitAt(2)
      val newTree = combine(toCombine(0), toCombine(1))
      buildTree(newTree :: rest)
  }

  def apply(freqChars: Seq[FreqChar]) = {
    require(freqChars.size > 0)
    val leafList: List[Leaf] = freqChars.map(fc => Leaf(fc._2, fc._1)).toList
    buildTree(leafList)
  }


  implicit val treeOrdering = new Ordering[FrequencyTree] {
    override def compare(x: FrequencyTree, y: FrequencyTree): Int = x.freq.compareTo(y.freq)
  }

}
