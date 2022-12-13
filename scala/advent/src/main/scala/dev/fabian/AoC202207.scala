package dev.fabian

import scala.annotation.tailrec

import cats.data.NonEmptyList
import cats.effect.{IO, IOApp}
import cats.parse.Parser

object AoC202207 extends IOApp.Simple {
  // Structures

  sealed trait Command

  sealed trait FS {
    def name: String
  }
  case class File(size: Int, name: String)       extends FS {
    // No Slashes in Name
    require(name.forall(c => c.isLetter || c == '.'))
  }
  case class Dir(name: String, content: Set[FS]) extends FS {
    // No Slashes in Name
    require(name.forall(_.isLetter))
    // No Name Clashes
    require(content.groupBy(_.name).forall(_._2.size == 1))
  }

  sealed trait CD               extends Command
  case object CDRoot            extends CD
  case object CDUp              extends CD
  case class CDDir(dir: String) extends CD

  case class LS(fs: NonEmptyList[FS]) extends Command

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

  def addInfo(wd: Seq[String], info: Set[FS], fs: Set[FS]): Set[FS] =
    wd match {
      case Nil              => fs union info
      case Seq(dirName, _*) =>
        val fsDir         = fs.filter(_.name == dirName)
        assert(fsDir.size <= 1)
        assert(fsDir.forall(_.isInstanceOf[Dir]))
        val dirContent    =
          fsDir.headOption.map(_.asInstanceOf[Dir].content).getOrElse(Set.empty)
        val newDirContent = Dir(dirName, addInfo(wd.drop(1), info, dirContent))
        fs.filterNot(_.name == dirName) + newDirContent
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

  override def run: IO[Unit] = for {
    input <- read("202207").compile.toList.map(_.mkString("\n"))

    parsedInput = commands.parse(input).toOption.get._2
    result      = interpret(parsedInput.toList)

    _ <- IO.println(result.map(_.name))
  } yield ()
}
