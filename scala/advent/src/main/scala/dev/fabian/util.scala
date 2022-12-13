package dev.fabian

import cats.effect.IO
import cats.parse.Parser
import fs2.io.file.{Files, Path}

def read(file: String) =
  Files[IO].readUtf8Lines(Path(s"../inputs/$file"))

// Util Parsers

val space: Parser[Unit]   = Parser.char(' ')
val newline: Parser[Unit] = Parser.char('\n')

val integer: Parser[Int] = Parser.charsWhile(_.isDigit).map(_.toInt)
val word: Parser[String] = Parser.charsWhile(_.isLetter)

val filename: Parser[String] =
  (word ~ (
    Parser.char('.') *> word
  ).map(x => s".$x").rep0.map(_.mkString))
    .map { case (base, ext) => base + ext }
