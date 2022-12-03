package dev.fabian

import cats.effect.unsafe.implicits.global

object AoC202201 extends App {
  val elves =
    read("202201")
      .split(_.isEmpty)
      .map(_.toList.map(_.toInt).sum)
      .compile
      .toList
      .unsafeRunSync()

  println(s"Maximum elf: ${elves.max}")
  println(s"Sum of max 3 elves: ${elves.sorted.takeRight(3).sum}")
}
