package com.github.simonthecat.sorting

import org.specs2.mutable._
import scala.collection.immutable.Seq

object QuicksortSpec extends Specification {
  "Quicksort " should {
    "sort empty sequence" in {
      val seq: Seq[Int] = Seq()
      val sorted: Seq[Int] = Seq()

      Quicksort.sort(seq) === sorted
    }

    "sort unity sequence" in {
      val seq: Seq[Int] = Seq(1)
      val sorted: Seq[Int] = Seq(1)

      Quicksort.sort(seq) === sorted
    }

    "sort already sorted sequence" in {
      val seq: Seq[Int] = Seq(-5, 10, 999)
      val sorted: Seq[Int] = Seq(-5, 10, 999)

      Quicksort.sort(seq) === sorted
    }

    "sort reverse sorted sequence" in {
      val seq: Seq[Int] = Seq(3,2,1)
      val sorted: Seq[Int] = Seq(1,2,3)

      Quicksort.sort(seq) === sorted
    }

    "sort random sequence" in {
      val seq: Seq[Int] = Seq(5, 7, 1, 9)
      val sorted: Seq[Int] = Seq(1, 5, 7, 9)

      Quicksort.sort(seq) === sorted
    }
  }
}
