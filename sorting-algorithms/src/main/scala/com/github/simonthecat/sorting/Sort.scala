package com.github.simonthecat.sorting

import scala.collection.immutable.Seq

trait Sort {
  def sort[T](seq: Seq[T])(implicit ordering: Ordering[T]): Seq[T]

  implicit def toOrdered[T](obj: T)(implicit ordering: Ordering[T]): Ordered[T] =
    new Ordered[T] {
      override def compare(that: T): Int = ordering.compare(obj, that)
    }

}
