package com.github.simonthecat.hanoi

import scala.annotation.tailrec
import scala.collection.immutable.Seq

case class HanoiTowers(position: Int, towers: Seq[Seq[Int]]) {
  val num = towers.map(_.size).sum

  private[this] def canPut(block: Int, tower: Seq[Int]) =
    tower.isEmpty || tower.head > block


  private[this] def tryPut(from: Int, to: Int): Option[HanoiTowers] = {
    if (canPut(towers(from).head, towers(to))) {
      val newTowers: Seq[Seq[Int]] = towers.zipWithIndex.map { case (tower, index) =>
        if (index == position) tower.tail
        else if (index == to) towers(from).head +: tower
        else tower
      }
      Some(new HanoiTowers((to + 1) % 3, newTowers))
    } else {
      None
    }
  }

  private[this] def mkStep(dir: Int): HanoiTowers = {
    if (towers(position).isEmpty)
      new HanoiTowers((position + 1) % 3, towers)
    else
      tryPut(position, (3 + position + dir) % 3)
        .orElse(tryPut(position, (3 + position + 2 * dir) % 3))
        .getOrElse(new HanoiTowers((3 + position + dir) % 3, towers))
  }

  @tailrec
  final def solve: HanoiTowers = {
    if (solved) this
    else this.step.solve
  }

  /**
   * For even sized problem moves blocks "right" (+1),
   * for odd the direction is "left" (-1).
   *
   * @return solution HanoiTowers
   */
  def step: HanoiTowers = num % 2 match {
    case 0 => mkStep(1)
    case 1 => mkStep(-1)
  }

  def solved: Boolean = towers(2).size == num
}

object HanoiTowers {
  def apply(num: Int) = {
    require(num >= 0)
    val initialTower = 1 to num
    val towers = Seq(initialTower, Seq(), Seq())
    new HanoiTowers(0, towers)
  }
}