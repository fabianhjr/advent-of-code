package dev.fabian

import cats.effect.{IO, IOApp}

object AoC202201 extends IOApp.Simple {
  val run = for {
    elves <- read("202201")
      .split(_.isEmpty)
      .map(_.toList.map(_.toInt).sum)
      .compile
      .toList
    _ <- IO.println(s"Maximum elf: ${elves.max}")
    _ <- IO.println(s"Sum of max 3 elves: ${elves.sorted.takeRight(3).sum}")
  } yield ()
}
