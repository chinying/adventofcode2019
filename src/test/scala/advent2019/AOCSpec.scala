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
