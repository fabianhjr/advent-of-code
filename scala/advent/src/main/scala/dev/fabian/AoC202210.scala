package dev.fabian

import scala.annotation.tailrec
import scala.math.abs

import cats.effect.{IO, IOApp}
import cats.parse.Parser

object AoC202210 extends IOApp.Simple {
  // Structures

  sealed trait Command
  case object Noop        extends Command
  case class Addx(x: Int) extends Command

  // Parsers

  val noop: Parser[Noop.type]  =
    Parser.string("noop").map(_ => Noop)
  val addx: Parser[Addx]       =
    (Parser.string("addx") *> space *> integer).map(Addx.apply)
  val command: Parser[Command] =
    noop | addx

  // Function
  @tailrec
  def interpret(
      commands: Seq[Command],
      signal: Seq[Int] = Nil,
      state: Int = 1
  ): Seq[Int] =
    commands match {
      case Nil               => signal
      case Seq(Noop, xs*)    =>
        interpret(xs, signal :+ state, state)
      case Seq(Addx(x), xs*) =>
        interpret(xs, signal :+ state :+ state, state = state + x)
    }

  override def run: IO[Unit] = for {
    inputs  <- read("202210")
                 .filter(_.nonEmpty)
                 .map(command.parse)
                 .compile
                 .toList
    commands = inputs.map(_.toOption.get._2)
    result   = interpret(commands)

    part1 = Seq(20, 60, 100, 140, 180, 220).map(i => i * result(i - 1)).sum
    part2 = result.zipWithIndex
              .map { case (signal, idx) =>
                if (abs(signal - (idx % 40)) <= 1) '#' else '.'
              }
              .grouped(40)
              .map(_.mkString)
              .mkString("\n")

    _ <- IO.println(s"Part 1: $part1")
    _ <- IO.println(s"Part 2:\n$part2")
  } yield ()
}
