package advent2019

import java.lang.IllegalStateException

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable.ListBuffer

object Main extends App {
  // println(Day1.a())
  // println(Day1.b())
  // Day2.a
  // Day2.b
  // Day3.a
  // Day3.b
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

object Direction extends Enumeration {
  val UP = Value("U")
  val RIGHT = Value("R")
  val DOWN = Value("D")
  val LEFT = Value("L")
}

object Day3 {
  type Wires = (String, String)
  // (y, x), (i, j), (m, n), (vertical, horizontal)
  type Coordinates = (Int, Int)
  type Grid = HashSet[Coordinates]
  type CoordinatesWithDistance = (Coordinates, Int)

  val directions = HashMap[String, Coordinates](
    ("U" -> (1, 0)),
    ("R" -> (0, 1)),
    ("D" -> (-1, 0)),
    ("L" -> (0, -1))
  )

  def readFile(): Wires = {
    val src = Source.fromResource("3.txt").getLines
    val w1 = src.next
    val w2 = src.next
    (w1, w2)
  }

  def parseDir(vec: String): (Coordinates) = {
    val magnitude = vec.substring(1).toInt
    // (directions.get(vec(0).toString), magnitude)
    val dir = vec(0).toString
    dir match {
      case "U" => (directions(dir)._1 * magnitude, directions(dir)._2)
      case "R" => (directions(dir)._1, directions(dir)._2 * magnitude)
      case "D" => (directions(dir)._1 * magnitude, directions(dir)._2)
      case "L" => (directions(dir)._1, directions(dir)._2 * magnitude)
      case _ => throw new IllegalStateException
    }
  }

  def pointsBetween(a: Coordinates, b: Coordinates) = {
    val hiX = if (a._1 > b._1) a._1 else b._1
    val loX = if (a._1 <= b._1) a._1 else b._1
    val hiY = if (a._2 > b._2) a._2 else b._2
    val loY = if (a._2 <= b._2) a._2 else b._2

    for (x <- loX to hiX; y <- loY to hiY) yield(y, x)
  }

  @tailrec
  def traceWire(
    remainingWire: Iterator[String],
    startPoint: Coordinates,
    grid: Grid): Grid = {
    if (!remainingWire.hasNext) {
      grid
    } else {
      val vec = remainingWire.next
      val translateBy = parseDir(vec)
      val pointsToInsert = new collection.mutable.HashSet[Coordinates]
      val otherPoint = (
        (startPoint._1 + translateBy._1),
        (startPoint._2 + translateBy._2)
      )
      for (point <- pointsBetween(startPoint, otherPoint)) {
        pointsToInsert.add(point)
      }
      traceWire(remainingWire, otherPoint, grid ++ pointsToInsert)
    }
  }

  def manhatten(a: Coordinates, b: Coordinates) = {
    Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)
  }

  def findClosestIntersection(w1: Array[String], w2: Array[String]) = {
    val origin = (0, 0)
    val grid1 = traceWire(w1.iterator, origin, new Grid)
    val grid2 = traceWire(w2.iterator, origin, new Grid)

    val intersections = (grid1 intersect grid2) - origin
    println(intersections)
    val closest = intersections.map(pt => manhatten(pt, origin))
      .toArray
      .sorted
      .head

    closest
  }

  def a() {
    val (i1, i2) = readFile
    val w1 = i1.split(",")
    val w2 = i2.split(",")
    // you start from (0, 0)
    println(findClosestIntersection(w1, w2))
  }

  def traceWire2(
    remainingWire: Iterator[String],
    startPoint: Coordinates,
    steps: Int,
    grid: HashMap[Coordinates, Int]
  ): HashMap[Coordinates, Int] = {
    if (!remainingWire.hasNext) {
      grid
    } else {
      val vec = remainingWire.next
      val translateBy = parseDir(vec)
      val pointsToInsert = new collection.mutable.HashMap[Coordinates, Int]
      val endPoint = (
        (startPoint._1 + translateBy._1),
        (startPoint._2 + translateBy._2)
      )

      // ASSUMPTION: each of these 4 are discrete, should refactor
      if (startPoint._1 > endPoint._1) {
        (startPoint._1 to endPoint._1 by -1).zipWithIndex.foreach({case (y: Int, idx: Int) => {
          pointsToInsert += ((y, startPoint._2) -> (idx + steps))
        }})
      }

      else if (startPoint._1 < endPoint._1) {
        (startPoint._1 to endPoint._1).zipWithIndex.foreach({case (y: Int, idx: Int) => {
          pointsToInsert += ((y, startPoint._2) -> (idx + steps))
        }})
      }

      else if (startPoint._2 > endPoint._2) {
        (startPoint._2 to endPoint._2 by -1).zipWithIndex.foreach({case (x: Int, idx: Int) => {
          pointsToInsert += ((startPoint._1, x) -> (idx + steps))
        }})
      }

      else if (startPoint._2 < endPoint._2) {
        (startPoint._2 to endPoint._2).zipWithIndex.foreach({case (x: Int, idx: Int) => {
          pointsToInsert += ((startPoint._1, x) -> (idx + steps))
        }})
      }

      // println(pointsToInsert.toList.sortBy(_._2))

      // path to next point
      traceWire2(remainingWire, endPoint, steps + pointsToInsert.size - 1, grid ++ pointsToInsert)
    }
  }

  private def combinedSteps(a: HashMap[Coordinates, Int], b: HashMap[Coordinates, Int], point: Coordinates) = {
    a(point) + b(point)
  }

  def b() {
    val (i1, i2) = readFile
    val w1 = i1.split(",")
    val w2 = i2.split(",")

    // val w1 = "R8,U5,L5,D3".split(",")
    // val w2 = "U7,R6,D4,L4".split(",")

    // val w1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",")
    // val w2 = "U62,R66,U55,R34,D71,R55,D58,R83".split(",")
    // val w1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(",")
    // val w2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",")

    val origin = (0, 0)
    val steps1 = (traceWire2(w1.iterator, origin, 0, new HashMap[Coordinates, Int]))
    val steps2 = (traceWire2(w2.iterator, origin, 0, new HashMap[Coordinates, Int]))
    val intersections = steps1.keySet.intersect(steps2.keySet) - origin
    println(
      intersections.map(i =>
        (i, combinedSteps(steps1, steps2, i))
      )
      .toList
      .sortBy(i => i._2) // should equal 410
    )
  }
}
