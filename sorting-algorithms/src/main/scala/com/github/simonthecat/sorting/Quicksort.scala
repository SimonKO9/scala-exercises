package com.github.simonthecat.sorting

import scala.collection.immutable.Seq

object Quicksort extends Sort {
  override def sort[T](seq: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {
    seq match {
      case Seq() => Seq()
      case _ =>
        if (seq.tail.isEmpty) seq
        else {
          val pivot = seq.head
          val (left, right) = seq.tail.partition(_ < pivot)
          sort(left :+ pivot) ++ sort(right)
        }
    }
  }
}
