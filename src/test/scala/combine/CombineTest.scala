package combine

import org.scalatest.{Matchers, path}
import Combine._

class CombineTest extends path.FunSpec with Matchers {


  describe("Compute permutations") {

    it("on a list of Int's") {
      permutations(Set(1, 2, 3)) shouldEqual Set(List(1, 2, 3), List(1, 3, 2), List(2, 1, 3), List(2, 3, 1), List(3, 2, 1), List(3, 1, 2))
    }

    it("permutation count is n!") {
      countPermutations(Set(1, 2, 3)) shouldEqual factorial(3)
      countPermutations(Set(1, 2, 3, 4, 5, 6)) shouldEqual factorial(6)
    }

    it("permutations with generic types") {
      permutations(Set[String]("one", "two")) shouldEqual Set(List("one", "two"), List("two", "one"))
    }
  }

  describe("Compute combinations and subsets") {

    it("combinations n C k") {
      combinationsCk(Set(1, 2, 3), 2) shouldEqual Set(Set(1, 2), Set(1, 3), Set(2, 3))
    }

    it("combinations of all subsets") {
      combinations(Set(1, 2)) shouldEqual Set(Set(), Set(1), Set(2), Set(1, 2))
    }

    it("combinations of all subsets with a longer set") {
      combinations(Set(1, 2, 3)) shouldEqual Set(Set(), Set(1), Set(2), Set(3), Set(1, 2), Set(2, 3), Set(1, 3), Set(1, 2, 3))
    }

    it("combinations alternative implemenration of all subsets with a longer set") {
      combinationsAlt(Set(1, 2, 3)) shouldEqual Set(Set(), Set(1), Set(2), Set(3), Set(1, 2), Set(2, 3), Set(1, 3), Set(1, 2, 3))
    }

    it("count possible combinations") {
      combinationsCount(3, 2) shouldEqual 3
    }

    it("a more complex count") {
      combinationsCount(6, 3) shouldEqual combinationsCk(Set(1, 2, 3, 4, 5, 6), 3).size
    }
  }

  describe("zip-like-sorta permutations with variable size, variable length lists") {

    it("a simple case") {
      Combine.variableZip(List(List(1), List(2, 3))) shouldEqual List(List(1, 2), List(1, 3))
      Combine.variableZip(List(List(1, 2), List(3))) shouldEqual List(List(1, 3), List(2, 3))
    }

    it("a more complex case") {
      Combine.variableZip(List(List(1, 2), List(3, 4), List(5))) shouldEqual List(List(1, 3, 5), List(1, 4, 5), List(2, 3, 5), List(2, 4, 5))
    }
  }
}