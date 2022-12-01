package dev.fabian

import java.io.EOFException
import scala.io.StdIn.*
import scala.util.Try

import cats.data.State

object AoC202201 extends App {
  enum Cond:
    case More
    case EOF

  def readElf(): (Cond, Long) =
    Try(readLong()).toEither match {
      case Right(calories) =>
        val (cond, otherCalories) = readElf()
        (cond, calories + otherCalories)
      // TODO: remove `e.getMessage().contains("END")` when better redirect / EOF is available
      case Left(e: NumberFormatException) if !e.getMessage().contains("END") =>
        (Cond.More, 0)
      case Left(e: NumberFormatException) => (Cond.EOF, 0)
      case Left(e: EOFException)          => (Cond.EOF, 0)
      case Left(e)                        => throw e
    }

  def readElves(): Seq[Long] =
    readElf() match {
      case (Cond.More, elf) => elf +: readElves()
      case (Cond.EOF, elf)  => Seq(elf)
    }

  val elves = readElves()
  println(s"Maximum elf: ${elves.max}")
  println(s"Sum of max 3 elves: ${elves.sorted.takeRight(3).sum}")
}
