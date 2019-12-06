package advent2019

import org.scalatest._

class Day1Spec extends FlatSpec with Matchers {
  "Day1 part a" should "calculate" in {
    Day1.calc(12) shouldEqual 2
    Day1.calc(14) shouldEqual 2
  }

  "Day1 part b" should "calculate" in {
    Day1.fuelForFuel(14, 0) shouldEqual 2
    Day1.fuelForFuel(1969, 0) shouldEqual 966
  }
}

class Day3Spec extends FlatSpec with Matchers {
  "Day 3 part a" should "compute" in {
    val w1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",")
    val w2 = "U62,R66,U55,R34,D71,R55,D58,R83".split(",")
    Day3.compute_a(w1, w2) shouldEqual 159
  }
}
