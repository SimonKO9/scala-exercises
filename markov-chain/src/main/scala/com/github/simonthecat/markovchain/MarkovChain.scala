package com.github.simonthecat.markovchain

import scala.annotation.tailrec
import scala.util.Random

case class MarkovChain[T](chains: Map[Seq[T], Seq[(T)]]) {
  require(chains.nonEmpty)

  val prefixLen = chains.head._1.size


  final def generate(start: Seq[T], limit: Int = 100): Seq[T] = {
    @tailrec
    def generate0(start: Seq[T], limit0: Int, acc: Seq[T]): Seq[T] = {
      if (limit0 <= 0) acc
      else
        next(start) match {
          case Some(n) =>
            generate0(start.tail :+ n, limit0 - 1, acc :+ n)
          case None =>
            acc
        }
    }

    start ++ generate0(start, limit, Seq())
  }

  def next(from: Seq[T]): Option[T] = {
    def pickCandidate(seq: Seq[(T)]) = {
      seq(Random.nextInt(seq.size))
    }

    val candidatesOpt = chains.get(from)
    candidatesOpt match {
      case Some(candidates) =>
        val candidate = pickCandidate(candidates)
        Some(candidate)
      case None =>
        None
    }
  }
}

object MarkovChain {
  def apply[T](seq: Seq[T], windowSize: Int): MarkovChain[T] = {
    val parts = seq.sliding(windowSize + 1).toSeq
    val keyValuePairs = parts map { part =>
      val (key, valueSeq) = part.splitAt(windowSize)
      val value = valueSeq.head
      (key, value)
    }
    val keyValueMap = keyValuePairs.groupBy {
      case (key, value) => key
    } mapValues {
      case keyValueSeq => keyValueSeq.map(kv => kv._2)
    }

    new MarkovChain[T](keyValueMap)
  }
}
