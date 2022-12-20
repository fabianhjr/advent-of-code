package dev.fabian

import scala.annotation.tailrec
import scala.math.{abs, signum}

import cats.effect.{IO, IOApp}
import cats.parse.Parser

object AoC202209 extends IOApp.Simple {
  // Structures

  enum Direction:
    case Up, Down, Left, Right
  import Direction.*

  case class Command(dir: Direction, times: Int)

  // Parsers

  val command =
    ((Parser.charIn(Set('U', 'D', 'L', 'R')) <* space) ~ integer).map {
      case (c, i) =>
        Command(
          c match {
            case 'U' => Up
            case 'D' => Down
            case 'L' => Left
            case 'R' => Right
          },
          i
        )
    }

  // Functions

  @tailrec
  def updateKnots(
      headPosition: (Int, Int),
      knotPositions: Seq[(Int, Int)],
      updatedKnotPositions: Seq[(Int, Int)] = Nil
  ): Seq[(Int, Int)] =
    knotPositions match {
      case Nil                       => updatedKnotPositions
      case Seq(knotPosition, knots*) =>
        val knotVector      = (
          headPosition._1 - knotPosition._1,
          headPosition._2 - knotPosition._2
        )
        val knotMovement    =
          if (abs(knotVector._1) > 1 || abs(knotVector._2) > 1)
            (signum(knotVector._1), signum(knotVector._2))
          else (0, 0)
        val newKnotPosition = (
          knotPosition._1 + knotMovement._1,
          knotPosition._2 + knotMovement._2
        )
        updateKnots(
          newKnotPosition,
          knots,
          updatedKnotPositions :+ newKnotPosition
        )
    }

  @tailrec
  def interpret(
      commands: Seq[Command],
      headPosition: (Int, Int) = (0, 0),
      knotPositions: Seq[(Int, Int)] = Seq((0, 0)),
      tailVisited: Set[(Int, Int)] = Set.empty
  ): Set[(Int, Int)] = commands match {
    case Nil                         => tailVisited
    case Seq(c, cs*) if c.times > 0  =>
      val newHeadPosition = c.dir match {
        case Up    => (headPosition._1, headPosition._2 + 1)
        case Down  => (headPosition._1, headPosition._2 - 1)
        case Left  => (headPosition._1 - 1, headPosition._2)
        case Right => (headPosition._1 + 1, headPosition._2)
      }
      val updatedKnots    = updateKnots(newHeadPosition, knotPositions)
      val tailPosition    = updatedKnots.last
      interpret(
        c.copy(times = c.times - 1) +: cs,
        newHeadPosition,
        updatedKnots,
        tailVisited + tailPosition
      )
    case Seq(c, cs*) if c.times == 0 =>
      interpret(cs, headPosition, knotPositions, tailVisited)
  }

  override def run = for {
    input <- read("202209").filter(_.nonEmpty).compile.toList
    parsed = input.map(command.parse).map(_.toOption.get._2)

    part1 = interpret(parsed)
    part2 = interpret(parsed, (0, 0), Seq.fill(9)((0, 0)))

    _ <- IO.println(s"Part 1: ${part1.size}")
    _ <- IO.println(s"Part 2: ${part2.size}")
  } yield ()
}
