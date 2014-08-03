package com.github.simonthecat.sorting

import org.specs2.mutable._
import scala.collection.immutable.Seq

object BubblesortSpec extends Specification {
  "Bubblesort " should {
    "sort empty sequence" in {
      val unsorted: Seq[Int] = Seq()
      val sorted: Seq[Int] = Seq()
      Bubblesort.sort(unsorted) === sorted
    }

    "sort unity sequence" in {
      val unsorted: Seq[Int] = Seq(5)
      val sorted: Seq[Int] = Seq(5)
      Bubblesort.sort(unsorted) === sorted
    }

    "sort already ordered sequence" in {
      val unsorted: Seq[Int] = Seq(-5,10,999)
      val sorted: Seq[Int] = Seq(-5, 10, 999)
      Bubblesort.sort(unsorted) === sorted
    }

    "sort reverse sorted sequence" in {
      val unsorted: Seq[Int] = Seq(5, -10, -20)
      val sorted: Seq[Int] = Seq(-20, -10, 5)
      Bubblesort.sort(unsorted) === sorted
    }

    "sort random sequence" in {
      val unsorted: Seq[Int] = Seq(5, 3, -1, 10, 2)
      val sorted: Seq[Int] = Seq(-1, 2, 3, 5, 10)
      Bubblesort.sort(unsorted) === sorted
    }
  }
}
