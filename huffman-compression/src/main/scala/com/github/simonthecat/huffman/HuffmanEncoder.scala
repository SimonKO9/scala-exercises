package com.github.simonthecat.huffman

abstract class HuffmanEncoder[T](freqTree: FrequencyTree) {

  def encode(chars: Seq[Char]) = {
    def encode0(char: Char, tree: FrequencyTree): Seq[Decision] = tree match {
      case Node(_, left, right, _) =>
        val (side, dec) =
          if (left.letterSet.contains(char)) (left, Left)
          else if (right.letterSet.contains(char)) (right, Right)
          else throw new IllegalArgumentException("Letter not in frequency tree")
        dec +: encode0(char, side)
      case _ => Seq()
    }

    val decisions: Seq[Decision] = chars flatMap (encode0(_, freqTree))
    output(decisions)
  }

  protected def output(decisions: Seq[Decision]): T

}

object HuffmanEncoder {
  def apply(freqTree: FrequencyTree): HuffmanEncoder[String] = apply(freqTree, defaultStringTransformer)

  def apply[T](freqTree: FrequencyTree, outputTransformer: (Seq[Decision]) => T): HuffmanEncoder[T] =
    new HuffmanEncoder[T](freqTree) {
      override protected def output(decisions: Seq[Decision]): T = outputTransformer(decisions)
    }

  private val defaultStringTransformer: (Seq[Decision] => String) = { decisions =>
    decisions.map {
      case Left => "L"
      case Right => "R"
    }.mkString
  }
}