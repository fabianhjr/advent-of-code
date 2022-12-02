package dev.fabian

import scala.util.Try
import scala.util.Success
import scala.io.StdIn.*

object AoC202202 extends App {
  enum Opponent:
    case A extends Opponent
    case B extends Opponent
    case C extends Opponent
  enum Mine(val value: Int):
    case X extends Mine(1)
    case Y extends Mine(2)
    case Z extends Mine(3)

  import Opponent.*
  import Mine.*

  def mapOpponent(o: Char): Opponent =
    o match {
      case 'A' => A
      case 'B' => B
      case 'C' => C
    }

  def mapMine(m: Char): Mine =
    m match {
      case 'X' => X
      case 'Y' => Y
      case 'Z' => Z
    }

  def scoreA(o: Opponent, m: Mine) =
    ((o, m) match {
      // Win
      case (A, Y) => 6
      case (B, Z) => 6
      case (C, X) => 6
      // Draw
      case (A, X) => 3
      case (B, Y) => 3
      case (C, Z) => 3
      // Lose
      case (A, Z) => 0
      case (B, X) => 0
      case (C, Y) => 0
    }) + m.value

  def scoreB(o: Opponent, m: Mine) =
    (o, m) match {
      // Lose
      case (A, X) => 3
      case (B, X) => 1
      case (C, X) => 2
      // Draw
      case (A, Y) => 4
      case (B, Y) => 5
      case (C, Y) => 6
      // Win
      case (A, Z) => 8
      case (B, Z) => 9
      case (C, Z) => 7
    }

  def readEntry(scoref: (Opponent, Mine) => Long): Try[Long] =
    Try(readLine()).map { case line: String =>
      val Array(o, m) = line.split(' ')
      scoref(mapOpponent(o.head), mapMine(m.head))
    }

  def loop(scoref: (Opponent, Mine) => Long): Long =
    readEntry(scoref) match {
      case Success(v) => v + loop(scoref)
      case _          => 0
    }

  println("Part 1")
  println(loop(scoreA))
  println("Part 2")
  println(loop(scoreB))
}
