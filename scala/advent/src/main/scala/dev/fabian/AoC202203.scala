package dev.fabian

import cats.effect.{IO, IOApp}

object AoC202203 extends IOApp.Simple {
  def priority(c: Char): Int =
    c.toInt - (if (c.isLower) 96 else 38)

  override def run: IO[Unit] = for {
    entries <- read("202203").filter(_.nonEmpty).compile.toList

    part1 = entries
              .map { e =>
                val (sackA, sackB) = e.splitAt(e.length() / 2)
                val c              = (sackA.toSet intersect sackB.toSet).head
                priority(c)
              }

    part2 = entries
              .grouped(3)
              .map {
                case Seq(e1, e2, e3) =>
                  val set = e1.toSet intersect e2.toSet intersect e3.toSet
                  val c   = set.head
                  priority(c)

                case _ => ???
              }

    _ <- IO.println(s"Part 1: ${part1.sum}")
    _ <- IO.println(s"Part 1: ${part2.sum}")
  } yield ()
}
