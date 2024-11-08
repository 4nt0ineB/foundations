package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(0, 1).swap == Pair(1, 0))
  }

  test("Pair map") {
    assert(Pair(0, 1).map(identity) == Pair(0, 1))
  }

  test("Pair decoded") {}

  test("Pair zipWith") {
    assert(Pair(0, 1).zipWith(Pair(1,2))((a,b) => a+b) == Pair(1,3))
  }

  test("Pair productNames") {}

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  def False[A]: Predicate[A] = Predicate(_ => false)
  def True[A]: Predicate[A] = Predicate(_ => true)

  test("Predicate &&") {
    assert((isEven && isPositive)(12) == true)
    assert((isEven && isPositive)(11) == false)
    assert((isEven && isPositive)(-4) == false)
    assert((isEven && isPositive)(-7) == false)
  }

  test("Predicate && BPT") {
    forAll{ (evalA: (Int => Boolean), evalB: (Int => Boolean), value: Int) =>
      val p1 = Predicate(evalA)
      val p2 = Predicate(evalB)

      assert((p1 && False)(value) == false)
      assert((p1 && True)(value) == p1(value))
    }
  }

  test("Predicate ||") {
    forAll{ (evalA: (Int => Boolean), evalB: (Int => Boolean), value: Int) =>
      val p1 = Predicate(evalA)
      val p2 = Predicate(evalB)

      assert((p1 || False)(value) == p1(value))
      assert((p1 || True)(value) == true)
    }
  }

  test("Predicate flip") {
    assert(Predicate.True.flip(()) == false)
    assert(Predicate.False.flip(()) == true)
  }

  test("isValidUser") {
    assert(isValidUser(User("John", 20)) == true)
    assert(isValidUser(User("John", 17)) == false)
    assert(isValidUser(User("john", 20)) == false)
    assert(isValidUser(User("x"   , 23)) == false)
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assert(userIdDecoder.decode("-1")  == UserId(-1))
    Try(userIdDecoder.decode("hello")).isFailure
  }

  test("JsonDecode UserId PBT round-trip") {
    forAll { (num: Int) =>
      val json = num.toString
      assert(userIdDecoder.decode(json) == UserId(num))
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26))
    Try(localDateDecoder.decode("2020-03-26")).isFailure
    Try(localDateDecoder.decode("hello")).isFailure
  }

  test("JsonDecoder round-trip") {
    forAll(genLocalDate) { (localDate: LocalDate) =>
      val dateString = DateTimeFormatter.ISO_LOCAL_DATE.format(localDate)
      val json = s"\"$dateString\""
      assert(localDateDecoder.decode(json) == localDate)
    }
  }

  test("JsonDecoder weirdLocalDateDecoder") {
    forAll(genLocalDate) { (localDate: LocalDate) =>
      val dateJson = "\"" +  DateTimeFormatter.ISO_LOCAL_DATE.format(localDate) + "\""
      val epochJson = localDate.toEpochDay.toString
      assert(weirdLocalDateDecoder.decode(dateJson) == localDate)
      assert(weirdLocalDateDecoder.decode(epochJson) == localDate)
    }
  }

  test("optionDecoder ") {
    assert(optionDecoder(stringDecoder).decode("null").isEmpty)
    val date = LocalDate.of(2020,3,26)
    assert(optionDecoder(weirdLocalDateDecoder).decode(date.toEpochDay.toString).get == date)
  }

  val genLocalDate: Gen[LocalDate] =
    Gen
      .choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay)
      .map(LocalDate.ofEpochDay)

  implicit val arbitraryLocalDate: Arbitrary[LocalDate] =
    Arbitrary(genLocalDate)

}
