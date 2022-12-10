package dev.fabian

import cats.effect.IOApp
import cats.effect.IO

object AoC202202 extends IOApp.Simple {
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

  def mapOpponent(o: String): Opponent =
    o match {
      case "A" => A
      case "B" => B
      case "C" => C
    }

  def mapMine(m: String): Mine =
    m match {
      case "X" => X
      case "Y" => Y
      case "Z" => Z
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

  override def run: IO[Unit] = for {
    entries <-
      read("202202")
        .filter(_.nonEmpty)
        .map { l =>
          val Array(os, ms) = l.split(' ')
          val (o, m)        = (mapOpponent(os), mapMine(ms))
          (scoreA(o, m), scoreB(o, m))
        }
        .compile
        .toList

    (part1, part2) = entries.unzip

    _ <- IO.println(s"Part 1: ${part1.sum}")
    _ <- IO.println(s"Part 2: ${part2.sum}")
  } yield ()
}
