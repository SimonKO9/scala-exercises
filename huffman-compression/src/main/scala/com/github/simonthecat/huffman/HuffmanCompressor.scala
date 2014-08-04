package com.github.simonthecat.huffman


sealed trait Decision

case object Left extends Decision

case object Right extends Decision

abstract class HuffmanCompressor[T](freqTree: FrequencyTree) {

  def compress(chars: Seq[Char]) = {
    def compress0(char: Char, tree: FrequencyTree): Seq[Decision] = tree match {
      case Node(_, left, right, _) =>
        val (side, dec) =
          if (left.letterSet.contains(char)) (left, Left)
          else if (right.letterSet.contains(char)) (right, Right)
          else throw new IllegalArgumentException("Letter not in frequency tree")
        dec +: compress0(char, side)
      case _ => Seq()
    }

    val decisions: Seq[Decision] = chars flatMap (compress0(_, freqTree))
    output(decisions)
  }

  protected def output(decisions: Seq[Decision]): T

}

object HuffmanCompressor {
  def apply(freqTree: FrequencyTree): HuffmanCompressor[String] = apply(freqTree, defaultStringTransformer)

  def apply[T](freqTree: FrequencyTree, outputTransformer: (Seq[Decision]) => T): HuffmanCompressor[T] =
    new HuffmanCompressor[T](freqTree) {
      override protected def output(decisions: Seq[Decision]): T = outputTransformer(decisions)
    }

  private val defaultStringTransformer: (Seq[Decision] => String) = { decisions =>
    decisions.map {
      case Left => "L"
      case Right => "R"
    }.mkString
  }
}