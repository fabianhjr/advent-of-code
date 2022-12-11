package dev.fabian

import cats.effect.IO
import cats.parse.Parser
import fs2.io.file.{Files, Path}

def read(file: String) =
  Files[IO].readUtf8Lines(Path(s"../inputs/$file"))

val integer: Parser[Int] = Parser.charsWhile(_.isDigit).map(_.toInt)
