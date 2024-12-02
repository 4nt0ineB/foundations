package exercises.action.fp.search

import exercises.action.DateGenerator.dateGen
import exercises.action.fp.IO
import exercises.action.fp.search.Airport.{londonGatwick, parisOrly}
import exercises.action.fp.search.SearchFlightGenerator._
import jdk.jshell.spi.ExecutionControl.ExecutionControlException
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant, LocalDate}
import scala.Ordering.Implicits._
import scala.concurrent.ExecutionContext
import scala.util.Random

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.fp.search.SearchResultTest
class SearchResultTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("cheapest, fastest and best are consistent with flights") {
    forAll(Gen.listOf(flightGen)) { (flights: List[Flight]) =>
      val result = SearchResult(flights)

      (result.cheapest, result.fastest, result.best) match {
        case (None, None, None)             => assert(result.flights.isEmpty)
        case (Some(f1), Some(f2), Some(f3)) => assert(result.flights.exists(Set(f1, f2, f3)))
        case _                              => fail("inconsistent")
      }
    }
  }

  test("cheapest is cheaper than any other flights") {
    forAll(Gen.listOf(flightGen)) { (flights: List[Flight]) =>
      val result = SearchResult(flights)

      for {
        cheapest <- result.cheapest
        flight   <- result.flights
      } assert(cheapest.unitPrice <= flight.unitPrice)
    }
  }

  test("fastest is faster than any other flights") {
    forAll(Gen.listOf(flightGen)) { (flights: List[Flight]) =>
      val result = SearchResult(flights)

      for {
        fastest <- result.fastest
        flight  <- result.flights
      } assert(fastest.duration <= flight.duration)
    }
  }

  test("apply - sort") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1a = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight1b = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 90.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val result = SearchResult(List(flight2, flight3, flight1a, flight1b, flight4))

    assert(result == SearchResult(List(flight1a, flight1b, flight2, flight3, flight4)))
  }

  test("fromTwoClients should handle errors gracefully") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight1)))
    val client2 = SearchFlightClient.constant(IO.fail(new Exception("unreachable")))

    val service = SearchFlightService.fromTwoClients(client1, client2)(ExecutionContext.global)
    val result = service.search(parisOrly, londonGatwick, today).attempt.unsafeRun()
    assert(result.isSuccess)
  }

  test("fromTwoClients should handle errors gracefully PBT") {
    forAll (airportGen, airportGen, dateGen, Gen.listOf(flightGen), Arbitrary.arbitrary[Throwable]) { (from, to, date, flights, error) =>

      val client1 = SearchFlightClient.constant(IO(flights))
      val client2 = SearchFlightClient.constant(IO.fail(error))

      val service = SearchFlightService.fromTwoClients(client1, client2)(ExecutionContext.global)
      val result = service.search(from, to, date).attempt.unsafeRun()
      assert(result.isSuccess)
    }
  }

  test("fromClients should handle errors gracefully clients PBT") {
    forAll (airportGen, airportGen, dateGen, Gen.listOf(clientGen)) { (from, to, date, clients) =>
      val service = SearchFlightService.fromClients(clients)(ExecutionContext.global)
      val result = service.search(from, to, date).attempt.unsafeRun()
      assert(result.isSuccess)
    }
  }

  test("fromClients - clients order doesn't matter") {
    forAll (airportGen, airportGen, dateGen, Gen.listOf(clientGen)) { (from, to, date, clients) =>
      val shuffledClients = Random.shuffle(clients)

      val service1 = SearchFlightService.fromClients(clients)(ExecutionContext.global)
      val service2 = SearchFlightService.fromClients(shuffledClients)(ExecutionContext.global)

      val result1 = service1.search(from, to, date).attempt.unsafeRun()
      val result2 = service2.search(from, to, date).attempt.unsafeRun()

      assert(result1 == result2)
    }
  }

}