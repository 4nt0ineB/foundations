package exercises.dataprocessing

import exercises.dataprocessing.StackSafeRecursiveExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class StackSafeRecursiveExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  val largeSize = 100000

  test("unsafeSum is not stack-safe") {
    try {
      unsafeSum(List.fill(largeSize)(0))
      fail("Expected stack overflow")
    } catch {
      case _: StackOverflowError => succeed
      case e: Throwable          => fail(e)
    }
  }

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
    assert(sum(List.fill(largeSize)(0)) == 0)
  }

  test("sum is consistent with std library") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("min is lte than other elements") {
    forAll { (numbers: List[Int]) =>
      for {
        minValue <- min(numbers)
        number <- numbers
      } assert(minValue <= number)
    }
  }

  test("min belongs to the list") {
    forAll { (numbers: List[Int]) =>
      assert(min(numbers) == numbers.minOption)
    }
  }

  test("reverse") {
    forAll { (numbers: List[Int]) =>
      assert(numbers.reverse == reverse(numbers))
    }
  }

  test("foldLeft") {
    forAll { (numbers: List[Int]) =>
      val sameNumbers = foldLeft(numbers, List.empty[Int])((l, num) => l :+ num)
      assert(sameNumbers == numbers)
    }
  }

}
