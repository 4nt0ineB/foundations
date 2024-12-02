package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderError.{EmptyBasket, InvalidStatus, MissingDeliveryAddress}
import exercises.errorhandling.project.OrderGenerator._
import exercises.errorhandling.project.OrderStatus.{Checkout, Delivered, Draft, Submitted}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant}

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val order = Order(
      id = OrderId("AAA"),
      status = Draft(basket=List(Item(ItemId("A1"), 2, 12.99))),
      createdAt = Instant.now(),
    )

    order.checkout match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Checkout(basket = NEL(Item(ItemId("A1"), 2, 12.99)),
        deliveryAddress = None))
    }
  }

  test("checkout empty basket example") {
    val order = Order(
      id = OrderId("AAA"),
      status = Draft(basket = Nil),
      createdAt = Instant.now(),
    )

    assert(order.checkout == Left(EmptyBasket))
  }

  test("checkout invalid status example") {
    val order = Order(
      id = OrderId("AAA"),
      status = Checkout(basket=NEL(Item(ItemId("A1"), 2, 12.99)), deliveryAddress = None),
      createdAt = Instant.now(),
    )

    assert(order.checkout == Left(InvalidStatus(order.status)))
  }

  test("submit successful example") {
    val order = Order(
      id = OrderId("AAA"),
      status = Checkout(basket = NEL(Item(ItemId("A1"), 2, 12.99)), deliveryAddress = Some(Address(12, "E16 8TR"))),
      createdAt = Instant.now(),
    )

    val submittedAt = Instant.now()
    order.submit(submittedAt) match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Submitted(basket = NEL(Item(ItemId("A1"), 2, 12.99)), deliveryAddress = Address(12, "E16 8TR"), submittedAt = submittedAt))
    }
  }

  test("submit no address example") {
    val order = Order(
      id = OrderId("AAA"),
      status = Checkout(basket = NEL(Item(ItemId("A1"), 2, 12.99)), deliveryAddress = None),
      createdAt = Instant.now(),
    )

    assert(order.submit(Instant.now()) == Left(MissingDeliveryAddress)) // replace ??? by the error you created for that scenario
  }

  test("submit invalid status example") {
    val order = Order(
      id = OrderId("AAA"),
      createdAt = Instant.now(),
      status = Submitted(basket = NEL(Item(ItemId("A1"), 2, 12.99)), deliveryAddress = Address(12, "E16 8TR"),
        submittedAt = Instant.now().plusSeconds(2))
    )

    assert(order.submit(Instant.now()) == Left(InvalidStatus(order.status)))
  }

//  test("submit empty basket example") {
//    val order = Order(
//      id = "AAA",
//      status = Checkout(basket = Nil, deliveryAddress = ???),
//      basket = Nil,
//      deliveryAddress = Some(Address(12, "E16 8TR")),
//      createdAt = Instant.now(),
//      submittedAt = None,
//      deliveredAt = None
//    )
//
//    assert(order.submit(Instant.now()) == Left(EmptyBasket))
//  }

  test("happy path") {
    val orderId         = "ORD0001"
    val createdAt       = Instant.now()
    val submittedAt     = createdAt.plusSeconds(5)
    val deliveredAt     = submittedAt.plusSeconds(3600 * 30) // 30 hours
    val order           = Order.empty(OrderId(orderId), createdAt)
    val item1           = Item(ItemId("AAA"), 2, 24.99)
    val item2           = Item(ItemId("BBB"), 1, 15.49)
    val deliveryAddress = Address(23, "E16 8FV")

    val result = for {
      order         <- order.addItem(item1)
      order         <- order.addItem(item2)
      order         <- order.checkout
      order         <- order.updateDeliveryAddress(deliveryAddress)
      order         <- order.submit(submittedAt)
      orderDuration <- order.deliver(deliveredAt)
    } yield orderDuration

    assert(
      result.map(_._1) == Right(
        Order(
          id = OrderId(orderId),
          status = Delivered(basket=NEL(item1, item2), deliveryAddress, submittedAt, deliveredAt),
          createdAt = createdAt,
        )
      )
    )

    assert(result.map(_._2) == Right(Duration.ofHours(30)))
  }

  test("happy path GEN") {
    forAll(orderIdGen, instantGen, durationGen, durationGen, nelOf(itemGen), addressGen) {
      (orderId, createdAt, submittedDelay, deliveredDelay, items, deliveryAddress) =>
        val submittedAt     = createdAt.plus(submittedDelay)
        val deliveredAt     = submittedAt.plus(deliveredDelay)
        val order           = Order.empty(orderId, createdAt)

        val result = for {
          order         <- order.addItems(items)
          order         <- order.checkout
          order         <- order.updateDeliveryAddress(deliveryAddress)
          order         <- order.submit(submittedAt)
          orderDuration <- order.deliver(deliveredAt)
        } yield orderDuration

        assert(
          result.map(_._1) == Right(
            Order(
              id = orderId,
              status = Delivered(items, deliveryAddress, submittedAt, deliveredAt),
              createdAt = createdAt,
            )
          )
        )

        assert(result.map(_._2) == Right(deliveredDelay))
    }
  }

  test("checkout is not allowed if order is in checkout, submitted or delivered status") {
    forAll(Gen.oneOf(checkoutGen, submittedGen, deliveredGen))(order =>
      assert(order.checkout == Left(InvalidStatus(order.status)))
    )
  }

}