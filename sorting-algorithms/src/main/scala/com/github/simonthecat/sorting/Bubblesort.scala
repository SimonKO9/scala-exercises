package com.github.simonthecat.sorting

import scala.collection.immutable.Seq

object Bubblesort extends Sort {
  override def sort[T](seq: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {
    def bubble(elem: T, s: Seq[T]): Seq[T] = {
      if (s.isEmpty) Seq(elem)
      else {
        if (elem > s.head) s.head +: bubble(elem, s.tail)
        else elem +: bubble(s.head, s.tail)
      }
    }

    def sort0(unsorted: Seq[T], sorted: Seq[T]): Seq[T] = {
      if (unsorted.isEmpty) sorted
      else {
        val bubbled = bubble(unsorted.head, unsorted.tail)
        val (left, last) = bubbled.splitAt(bubbled.size - 1)
        sort0(left, last.head +: sorted)
      }

    }

    sort0(seq, Seq())
  }
}
