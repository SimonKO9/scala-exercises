package com.github.simonthecat.binarysearch

import org.specs2.mutable._

object BinarySearchSpec extends Specification {
  "BinarySearch" should {
    "return none for empty vector" in {
      val vec: Vector[Int] = Vector()
      BinarySearch(vec, 5) === None
      BinarySearch(vec, -1) === None
    }

    "return index for unity vector" in {
      val vec = Vector(5)
      BinarySearch(vec, 5) === Some(0)
    }

    "return none for non-existing value in unity vector" in {
      val vec = Vector(5)
      BinarySearch(vec, -1) === None
      BinarySearch(vec, 10) === None
    }

    "return none for non-existing value in (1,5,10)" in {
      val vec = Vector(1,5,10)
      BinarySearch(vec, -1) === None
      BinarySearch(vec, 9) === None
      BinarySearch(vec, 20) === None
    }

    "return correct value for sequence (1,2,3,4,5)" in {
      val vec = Vector(1,2,3,4,5)
      BinarySearch(vec, 1) === Some(0)
      BinarySearch(vec, 2) === Some(1)
      BinarySearch(vec, 3) === Some(2)
      BinarySearch(vec, 4) === Some(3)
      BinarySearch(vec, 5) === Some(4)
    }

    "return correct value for sequence (1,2,2,3)" in {
      val vec = Vector(1,2,2,3)
      BinarySearch(vec, 1) === Some(0)
      BinarySearch(vec, 3) === Some(3)
      BinarySearch(vec, 2) === Some(2)
    }

    "return correct value for sequence (-5, 17, 33, 60)" in {
      val vec = Vector(-5, 17, 33, 60)
      BinarySearch(vec, -5) === Some(0)
      BinarySearch(vec, 5) === None

      BinarySearch(vec, 17) === Some(1)
      BinarySearch(vec, 33) === Some(2)
      BinarySearch(vec, 60) === Some(3)
    }
  }
}
