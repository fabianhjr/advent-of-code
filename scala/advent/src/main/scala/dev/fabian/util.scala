package dev.fabian

import cats.effect.IO
import fs2.io.file.{Files, Path}

def read(file: String) =
  Files[IO].readUtf8Lines(Path(s"../inputs/$file"))
