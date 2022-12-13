package dev.fabian

import scala.annotation.tailrec

import cats.data.NonEmptyList
import cats.effect.{IO, IOApp}
import cats.parse.Parser
import cats.syntax.all.*

object AoC202205 extends IOApp.Simple {
  // Yard Data Parsers
  val empty: Parser[Unit] = space.rep(3, 3).void
  val crate: Parser[Char] =
    (Parser.char('[') *> Parser.charWhere(_.isLetter) <* Parser.char(']'))

  val crateSlot: Parser[Option[Char]] =
    crate.map(Some(_)) | empty.map(_ => None)

  val crateRow: Parser[NonEmptyList[Option[Char]]] = crateSlot.repSep(space)

  type Yard = Seq[List[Char]]

  val yard: Parser[Yard] =
    crateRow.repSep(newline).map { yard =>
      for {
        i <- 0 until yard.head.length

        stackWithMaybe = yard.map(_.toList(i))
        stack          = stackWithMaybe.filter(_.isDefined).map(_.get)
      } yield stack
    }

  // Command Prasers
  case class Command(move: Int, from: Int, to: Int)

  val command: Parser[Command] =
    (
      (Parser.string("move ") *> integer),
      (Parser.string(" from ") *> integer),
      (Parser.string(" to ") *> integer)
    ).mapN(Command.apply)

  // Interpreter
  @tailrec
  def interpret(yard: Yard, commands: Seq[Command], atOnce: Boolean): Yard =
    commands match {
      case Nil => yard

      case commands @ Seq(Command(0, f, t), _*) =>
        interpret(yard, commands.drop(1), atOnce)

      case commands @ Seq(Command(m, f, t), _*) =>
        val amountToMove = if (atOnce) m else 1
        assert(yard(f - 1).size >= amountToMove)

        val crates = yard(f - 1).take(amountToMove)

        val newYard = yard
          .updated(f - 1, yard(f - 1).drop(amountToMove))
          .updated(t - 1, crates ++ yard(t - 1))

        val newCommands = commands
          .updated(0, Command(m - amountToMove, f, t))

        interpret(newYard, newCommands, atOnce)
    }

  override def run: IO[Unit] = for {
    entries <- read("202205").split(_.isEmpty()).compile.toList

    List(yardEntry, operations) = entries

    parsedYard: Yard =
      yard.parse(yardEntry.dropRight(1).toList.mkString("\n")).toOption.get._2
    parsedCommands   =
      operations.map(command.parse(_)).map(_.toOption.get._2)

    result1 = interpret(parsedYard, parsedCommands.toList, atOnce = false)
    result2 = interpret(parsedYard, parsedCommands.toList, atOnce = true)

    _ <- IO.println("Part 1: " + result1.map(_.head).mkString)
    _ <- IO.println("Part 2: " + result2.map(_.head).mkString)
  } yield ()
}
