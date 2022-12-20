package dev.fabian

import scala.annotation.tailrec
import scala.math.max

import cats.effect.{IO, IOApp}

object AoC202208 extends IOApp.Simple {
  case class Tree(
      height: Int,
      visible: Boolean = false,
      score: Int = 1
  )

  type Forest = Seq[Seq[Tree]]

  @tailrec
  def checkVisibility(
      input: Seq[Tree],
      bound: Option[Int] = None,
      out: Seq[Tree] = Nil
  ): Seq[Tree] =
    input match {
      case Nil         => out
      case Seq(x, xs*) =>
        val markedVisibility: Tree =
          if (bound.fold(true)(x.height > _)) x.copy(visible = true)
          else x
        val newBound: Option[Int]  =
          bound.map(max(_, x.height)).orElse(Option(x.height))
        checkVisibility(xs, newBound, out :+ markedVisibility)
    }

  def checkHorizontalVisibility(input: Forest): Forest = for {
    i             <- 0 until input.length
    eastVisibility = checkVisibility(input(i))
    westVisibility = checkVisibility(eastVisibility.reverse).reverse
  } yield westVisibility

  def checkVerticalVisibility(input: Forest): Forest =
    checkHorizontalVisibility(input.transpose).transpose

  def score(tree: Tree, line: Seq[Tree]) = {
    val allVisible = line.takeWhile(_.height < tree.height).length
    val barrier    = line.drop(allVisible).take(1).size
    allVisible + barrier
  }

  def scoreHorizontal(input: Forest): Forest = for {
    i     <- 0 until input.length
    scored = input(i).zipWithIndex.map { case (tree, j) =>
               val newScore = score(tree, input(i).drop(j + 1)) *
                 score(tree, input(i).take(j).reverse)
               tree.copy(score = tree.score * newScore)
             }
  } yield scored

  def scoreVertical(input: Forest): Forest =
    scoreHorizontal(input.transpose).transpose

  override def run: IO[Unit] = for {
    input         <- read("202208")
                       .filter(_.nonEmpty)
                       .compile
                       .toList

    forest: Forest =
      input.map(_.map(c => Tree(c.toInt - 48)))

    checkedVisibility =
      checkVerticalVisibility(checkHorizontalVisibility(forest))
    visibleTrees      =
      checkedVisibility.flatten.count(_.visible == true)

    scored        = scoreVertical(scoreHorizontal(forest))
    highestScored = scored.flatten.map(_.score).max

    _ <- IO.println(s"Part 1: $visibleTrees")
    _ <- IO.println(s"Part 2: $highestScored")
  } yield ()
}
