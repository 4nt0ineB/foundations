package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import org.scalacheck.{Arbitrary, Gen}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0),
    )
    val parSamples = ParList.byPartitionSize(3, samples, ExecutionContext.global)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples, ExecutionContext.global)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minSampleByTemperature returns the coldest Sample no partition") {
    forAll { (samples: ParList[Sample]) =>
      assert(minSampleByTemperature(samples) == samples.toList.minByOption(sample => sample.temperatureFahrenheit))
    }
  }


  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0),
    )
    val parSamples = ParList.byPartitionSize(3, samples, ExecutionContext.global)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("averageTemperature PBT") {
    forAll { (samples: ParList[Sample]) =>
      averageTemperature(samples) match {
        case None => assert(samples.toList.isEmpty)
        case Some(avg) =>
          val newSamples = samples.map(sample => sample.copy(temperatureFahrenheit = sample.temperatureFahrenheit * 2))
          averageTemperature(newSamples) match {
            case None => fail("problem with map")
            case Some(avg2) => assert(avg * 2 - avg2 < 0.00001)
          }
      }

    }
  }

  test("monoFoldLeft is consistent with List sum") {
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.monoFoldLeft(Monoid.sum[Int]) == numbers.toList.sum)
    }
  }

  test("foldMap(identity) is consistent monoFoldLeft"){
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.foldMap(identity)(Monoid.sum[Int]) == numbers.map(identity).monoFoldLeft(Monoid.sum[Int]))
    }
  }

  val doubleGen: Gen[Double] = Gen.choose(-100.0f, 100.0f).map(_.toDouble)
  val intGen: Gen[Int] =  Gen.choose(Int.MinValue, Int.MaxValue)


  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = {
    PropertyCheckConfiguration(minSuccessful = 100)
  }

  checkMonoid("sumInt", Monoid.sum[Int], intGen)
  checkMonoid("sumDouble", Monoid.sum[Double], doubleGen)
  checkMonoid("zip", Monoid.zip(Monoid.sum[Int], Monoid.sum[Int]), Gen.zip(intGen, intGen))
  checkMonoid("minSample", Monoid.minSample, Gen.option(sampleGen))
  checkMonoid("maxSample", Monoid.maxSample, Gen.option(sampleGen))

  def checkMonoid[A](name: String, param: Monoid[A], gen: Gen[A]) = {
    test(s"monoFoldLeft ${name} - combine to be a no-op with default")  {
        forAll(gen) { (value: A) =>
          assert(param.combine(value, param.default) == value)
          assert(param.combine(param.default, value) == value)
        }
    }
    test(s"MonoFold ${name} - combine is associative") {
      forAll(gen, gen, gen) { (first: A, second: A, third: A) =>
        val oneWay = param.combine(first, param.combine(second, third))
        val otherWay = param.combine(param.combine(first, second), third)
        assert(oneWay == otherWay)
      }
    }
  }

  test("parFoldMap(identity) is consistent with foldMap") {
    forAll{ (samples: ParList[Int]) =>
//      val fixedPool = Executors.newFixedThreadPool(2)
//      val ec = ExecutionContext.fromExecutor(fixedPool)
      val monoid = Monoid.sum[Int]
      assert(samples.parFoldMap(identity)(monoid) == samples.foldMap(identity)(monoid))
    }
  }

  test("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples),
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }
}