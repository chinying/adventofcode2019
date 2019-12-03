package advent2019

import scala.io.Source
import scala.annotation.tailrec

object Main extends App {
  // println(Day1.a())
  // println(Day1.b())
  // Day2.a
  Day2.b
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

object Day2 {
  def a() {
    val src = Source.fromResource("2.txt").getLines
    val input = src.next.split(",").map(_.toInt).toList
    val instructions = input.grouped(4)
    var registers = input.toArray

    registers(1) = 12
    registers(2) = 2
    println(compute(instructions, registers))
  }

  def compute(instructions: Iterator[List[Int]], registers: Array[Int]): Int = {
    val instruction = instructions.next
    if (instruction.length != 4) {
      registers(0)
    } else {
      val op = instruction(0).toInt
      val r1 = instruction(1).toInt
      val r2 = instruction(2).toInt
      val dest = instruction(3).toInt
      op match {
        case 1 => registers(dest) = registers(r1) + registers(r2)
        case 2 => registers(dest) = registers(r1) * registers(r2)
        case 99 => registers
        case _ => throw new Exception("illegal state")
      }
      compute(instructions, registers)
    }
  }

  def b() {
    val src = Source.fromResource("2.txt").getLines
    val input = src.next.split(",").map(_.toInt).toList
    val registers = input.toArray

    val endloop = registers.length
    for (noun <- 0 to endloop) {
      for (verb <- 0 to endloop) {
        val instructions = input.grouped(4)
        var mutableRegisters = registers.toArray
        mutableRegisters(1) = noun
        mutableRegisters(2) = verb
        val output = compute(instructions, mutableRegisters)
        if (output == 19690720) {
          println(noun, verb)
          println(100 * noun + verb)
        }
      }
    }

  }
}