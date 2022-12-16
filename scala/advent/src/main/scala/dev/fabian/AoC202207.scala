package dev.fabian

import scala.annotation.tailrec
import scala.math.max

import cats.data.NonEmptyList
import cats.effect.{IO, IOApp}
import cats.parse.Parser

object AoC202207 extends IOApp.Simple {
  // Structures

  sealed trait Command

  sealed trait CD               extends Command
  case object CDRoot            extends CD
  case object CDUp              extends CD
  case class CDDir(dir: String) extends CD

  case class LS(content: NonEmptyList[FS]) extends Command

  sealed trait FS {
    def name: String
    def size: Int
  }

  case class File(size: Int, name: String)       extends FS
  case class Dir(name: String, content: Set[FS]) extends FS {
    lazy val size: Int = content.map(_.size).sum
  }

  // Parsers

  val commandStart: Parser[Unit] = Parser.string("$ ").void

  val changeDir: Parser[CD] = Parser.string("cd") *> space *> (
    Parser.char('/').map(_ => CDRoot) |
      Parser.string("..").map(_ => CDUp) |
      word.map(CDDir.apply)
  )

  val listOutputDir: Parser[Dir]   =
    (Parser.string("dir") *> space *> word).map(Dir(_, Set.empty))
  val listOutputFile: Parser[File] =
    (integer ~ (space *> filename)).map(File.apply)

  val listOutput: Parser[NonEmptyList[FS]] =
    (listOutputDir | listOutputFile).repSep(newline)

  val list: Parser[LS] =
    (Parser.string("ls") ~ newline) *> listOutput.map(LS.apply)

  val commands: Parser[NonEmptyList[Command]] =
    (commandStart *> (changeDir | list)).repSep(newline)

  // Functions

  def addInfo(wd: Seq[String], info: Set[FS], content: Set[FS]): Set[FS] =
    wd match {
      case Nil              => content union info
      case Seq(dirName, _*) =>
        val dirToUpdate = content
          .find(_.name == dirName)
          .fold(Set.empty)(_.asInstanceOf[Dir].content)
        val udatedDir   = Dir(dirName, addInfo(wd.drop(1), info, dirToUpdate))
        content.filterNot(_.name == dirName) + udatedDir
    }

  @tailrec
  def interpret(
      commands: Seq[Command],
      wd: Seq[String] = Nil,
      fs: Set[FS] = Set.empty
  ): Set[FS] =
    commands match {
      case Nil                 => fs
      case Seq(CDRoot, _*)     =>
        interpret(commands.drop(1), Nil, fs)
      case Seq(CDUp, _*)       =>
        interpret(commands.drop(1), wd.drop(1), fs)
      case Seq(CDDir(dir), _*) =>
        interpret(commands.drop(1), dir +: wd, fs)
      case Seq(LS(info), _*)   =>
        interpret(
          commands.drop(1),
          wd,
          addInfo(wd.reverse, info.toList.toSet, fs)
        )
    }

  def sumOfAllUnder100000(fs: Set[FS]): Int =
    fs.filter(_.isInstanceOf[Dir]).map(_.size).filter(_ <= 100000).sum +
      fs.filter(_.isInstanceOf[Dir])
        .map(_.asInstanceOf[Dir])
        .map(d => sumOfAllUnder100000(d.content))
        .sum

  def leastEnough(fs: Set[FS], targetSize: Int): Option[Dir] = {
    val dirs = fs
      .filter(_.isInstanceOf[Dir])
      .map(_.asInstanceOf[Dir])

    val thisLevel   = dirs
      .filter(_.size >= targetSize)
    val innerLevels = dirs
      .map(d => leastEnough(d.content, targetSize))
      .flatten

    (thisLevel union innerLevels).toSeq.sortBy(_.size).headOption
  }

  override def run: IO[Unit] = for {
    input <- read("202207").compile.toList.map(_.mkString("\n"))

    parsedInput = commands.parse(input).toOption.get._2
    result      = interpret(parsedInput.toList)

    part1 = sumOfAllUnder100000(result)

    totalUsed  = Dir("root", content = result).size
    needToFree = max(30000000 - (70000000 - totalUsed), 0)

    part2 = leastEnough(result, needToFree)

    _ <- IO.println(s"Part 1: $part1")
    _ <- IO.println(s"Part 2: ${part2.map(_.size)}")
  } yield ()
}
