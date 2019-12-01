package advent2019

import scala.io.Source
import scala.annotation.tailrec

object Main extends App {
  // println(Day1.a())
  println(Day1.b())
}

object Day1 {
  def a() = {
    val input = Source.fromResource("1.txt").getLines
    input.map(line => calc(line.toInt)).sum
  }

  def b() = {
    val input = Source.fromResource("1.txt").getLines
    input.map(line => fuelForFuel(line.toInt, 0)).sum
  }

  def calc(n: Int) = (n / 3 - 2)

  @tailrec
  def fuelForFuel(n: Int, acc: Int): Int = {
    val fuelNeeded = calc(n)
    if (fuelNeeded <= 0) acc
    else fuelForFuel(fuelNeeded, acc + fuelNeeded)
  }
}