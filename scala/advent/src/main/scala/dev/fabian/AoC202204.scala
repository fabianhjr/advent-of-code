package dev.fabian

import cats.effect.{IO, IOApp}
import cats.parse.Parser

object AoC202204 extends IOApp.Simple {
  case class Range(lowerBound: Int, upperBound: Int) {
    require(lowerBound <= upperBound)

    def eitherFullyContains(other: Range): Boolean =
      (lowerBound <= other.lowerBound && other.upperBound <= upperBound) ||
        (other.lowerBound <= lowerBound && upperBound <= other.upperBound)

    def overlaps(other: Range): Boolean =
      (upperBound >= other.upperBound && lowerBound <= other.upperBound) ||
        (lowerBound <= other.lowerBound && upperBound >= other.lowerBound) ||
        eitherFullyContains(other)
  }

  val range: Parser[Range]         =
    (integer ~ Parser.char('-') ~ integer)
      .map { case ((lb, ()), up) => Range(lb, up) }
  val pair: Parser[(Range, Range)] =
    (range ~ Parser.char(',') ~ range)
      .map { case ((ra, ()), rb) => (ra, rb) }

  override def run: IO[Unit] = for {
    entries <-
      read("202204")
        .filter(_.nonEmpty)
        .map { l =>
          val ((_, (rangeA, rangeB))) = pair.parse(l).toOption.get
          (rangeA, rangeB)
        }
        .compile
        .toList

    part1 =
      entries
        .filter { case (rangeA, rangeB) =>
          rangeA eitherFullyContains rangeB
        }
    part2 =
      entries
        .filter { case (rangeA, rangeB) =>
          rangeA overlaps rangeB
        }

    _ <- IO.println(s"Part 1: ${part1.length}")
    _ <- IO.println(s"Part 2: ${part2.length}")
  } yield ()
}
