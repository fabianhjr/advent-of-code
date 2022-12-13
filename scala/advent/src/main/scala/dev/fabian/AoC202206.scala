package dev.fabian

import scala.annotation.tailrec

import cats.effect.{IO, IOApp}
import cats.parse.{Parser, Parser0}

object AoC202206 extends IOApp.Simple {
  def anyNotIn(chars: Set[Char]): Parser[Char] =
    Parser.charWhere(c => !(chars contains c))

  def nDifferent(n: Int, notIn: Set[Char] = Set.empty): Parser[Int] =
    anyNotIn(notIn).flatMap { newChar =>
      if (n > 1) nDifferent(n - 1, notIn + newChar)
      else Parser.index
    }

  def find(n: Int): Parser[Int] = Parser.recursive { recurse =>
    nDifferent(n).backtrack | (Parser.anyChar *> recurse)
  }

  override def run: IO[Unit] = for {
    input <- read("202206").compile.toList.map(_.head)

    part1 = find(4).parse(input).toOption.get._2
    part2 = find(14).parse(input).toOption.get._2

    _ <- IO.println(s"Part 1: $part1")
    _ <- IO.println(s"Part 2: $part2")
  } yield ()
}
