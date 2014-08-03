package com.github.simonthecat.binarysearch

object BinarySearch {
  def apply[T](sequence: Seq[T], subject: T)(implicit ordering: Ordering[T]): Option[Int] = {
    def find(seq: Seq[T], offset: Int): Option[Int] = {
      if (seq.isEmpty) None
      else if (seq.tail.isEmpty) {
        if(seq.head == subject) Some(offset)
        else None
      } else {
        val index = seq.size / 2
        val elem = seq(index)
        val cmp = ordering.compare(subject, elem)
        if (cmp == 0) Some(offset + index)
        else if (cmp < 0) find(seq.view.take(index), offset)
        else find(seq.view.drop(index), offset + index)
      }
    }

    find(sequence, 0)
  }
}
