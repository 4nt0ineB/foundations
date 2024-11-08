package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits contains digits only") {
    forAll { (text: String) =>
      selectDigits(text).foreach(c => assert(c.isDigit))
    }
  }

  test("secret examples") {
    assert(secret("mysecretpassword") == "***************" )
  }

  test("secret length is equal") {
    forAll { (text: String) =>
      assert(secret(text).length == text.length)
    }
  }

  test("secret PBT") {
    forAll { (text: String) =>
      val once = secret(text)
      val twice = secret(text)
      assert(once == twice)
    }
  }

  test("isValidUsername reverse") {
    forAll { (username: String) =>
      assert(isValidUsername(username.reverse) == isValidUsername(username))
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("Point isPositive max 0") {
    forAll { (x: Int, y: Int, z:Int) =>
        assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test("Point isEven") {
    forAll({ (x: Int, y: Int, z: Int) =>
        assert(Point(x*2, y*2, z*2).isEven)
    })
  }

  test("Point forAll") {
    forAll({ (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      assert(Point(x: Int, y: Int, z: Int).forAll(predicate) == List(x,y,z).forall(predicate))
    })
  }
}
