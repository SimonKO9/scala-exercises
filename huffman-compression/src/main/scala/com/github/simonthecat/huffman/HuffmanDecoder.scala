package com.github.simonthecat.huffman

import scala.annotation.tailrec

abstract class HuffmanDecoder[T](freqTree: FrequencyTree) {

  def decode(input: Seq[T]): Seq[Char] = {
    def decodeChar(input: Seq[Decision], tree: FrequencyTree): (Char, Seq[Decision]) = tree match {
      case l: Leaf =>
        (l.letter, input)
      case n: Node =>
        if(input.isEmpty)
          throw new IllegalArgumentException("Input not decodable. Probably it was decoded using other frequency tree")

        input.head match {
          case Left => decodeChar(input.tail, n.left)
          case Right => decodeChar(input.tail, n.right)
        }
    }

    @tailrec
    def decode0(in: Seq[Decision], acc: Seq[Char]): Seq[Char] = in match {
      case Seq() => acc
      case _ =>
        val (char, rest) = decodeChar(in, freqTree)
        decode0(rest, char +: acc)
    }

    val decisions = inputReader(input)
    decode0(decisions, Seq()).reverse
  }

  protected def inputReader(input: Seq[T]): Seq[Decision]
}

object HuffmanDecoder {
  def apply[T](tree: FrequencyTree, reader: (Seq[T]) => Seq[Decision]): HuffmanDecoder[T] = new HuffmanDecoder[T](tree) {
    override protected def inputReader(input: Seq[T]): Seq[Decision] = reader(input)
  }

  def apply(tree: FrequencyTree): HuffmanDecoder[Char] = apply(tree, stringReaderLR)

  val stringReaderLR: (Seq[Char] => Seq[Decision]) = { chars =>
    chars.map {
      case 'L' => Left
      case 'R' => Right
      case _ => throw new IllegalArgumentException("String should contain only L's or R's")
    }
  }
}