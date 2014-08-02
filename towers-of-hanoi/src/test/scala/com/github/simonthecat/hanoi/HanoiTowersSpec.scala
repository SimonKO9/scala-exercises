package com.github.simonthecat.hanoi

import org.specs2.mutable._

class HanoiTowersSpec extends Specification {
  "The HanoiTowers" should {
    "throw an exception for negative values" in {
      HanoiTowers(-3) must throwAn[IllegalArgumentException]
      HanoiTowers(-1) must throwAn[IllegalArgumentException]
    }

    "should be solved for 0 sized" in {
      HanoiTowers(0).solved === true
    }

    " should have state ((2), (1), ()) after one step for size=2" in {
      val towers = HanoiTowers(2)
      val nextTowers = towers.step
      nextTowers.towers == Seq(Seq(2), Seq(1), Seq())
    }

    " should have state ((2), (1), ()) after 2 steps for size=2" in {
      val towers = HanoiTowers(2)
      val nextTowers = towers.step.step
      nextTowers.towers == Seq(Seq(2), Seq(1), Seq())
    }

    " should have state ((), (1), (2)) after 3 steps for size=2" in {
      val towers = HanoiTowers(2)
      val nextTowers = towers.step.step.step
      nextTowers.towers == Seq(Seq(), Seq(1), Seq(2))
    }

    " should be eventually solved for size=2" in {
      val towers = HanoiTowers(2)
      val solution = towers.solve
      solution.towers === Seq(Seq(), Seq(), Seq(1, 2))
      solution.solved mustEqual true
    }

    " should be eventually solved for size=3" in {
      val towers = HanoiTowers(3)
      val solution = towers.solve
      solution.towers === Seq(Seq(), Seq(), 1 to 3)
      solution.solved mustEqual true
    }

    " should be eventually solved for size=10" in {
      val size = 10
      val towers = HanoiTowers(size)
      val solution = towers.solve
      solution.towers === Seq(Seq(), Seq(), 1 to size)
      solution.solved mustEqual true
    }
  }
}
