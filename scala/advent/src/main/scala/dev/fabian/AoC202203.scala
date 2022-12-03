package dev.fabian

import scala.io.StdIn.*

object AoC202203 extends App {
  def common(input: String) = {
    val (sackA, sackB) = input.splitAt(input.length() / 2)
    val common = (sackA.toSet intersect sackB.toSet)
    common.head
  }

  def priority(c: Char): Int =
    c.toInt - (if (c.isLower) 96 else 38)

  def loopA(acc: Long): Long = {
    val input = readLine()
    if (input.isEmpty()) acc else loopA(acc + priority(common(input)))
  }

  def loopB(acc: Long): Long = {
    val inputA = readLine()
    if (inputA.isEmpty()) acc
    else {
      val inputB = readLine()
      val inputC = readLine()
      val common =
        (inputA.toSet intersect inputB.toSet intersect inputC.toSet).head

      loopB(acc + priority(common))
    }
  }

  println(loopA(0))
  println(loopB(0))
}
