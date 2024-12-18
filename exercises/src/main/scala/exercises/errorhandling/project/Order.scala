package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderStatus.{Checkout, Delivered, Draft, Submitted}

import java.time.{Duration, Instant}

case class OrderId(value: String)
case class ItemId(value: String)

case class Order(
  id: OrderId,
  status: OrderStatus,
  createdAt: Instant,               // set when the order is created ("Draft")
) {

  // Adds an `Item` to the basket.
  // This action is only permitted if the `Order` is in "Draft" or "Checkout" statuses.
  // If the `Order` is in "Checkout" status, move it back to "Draft".
  // Note: We don't verify if the `Item` is already in the basket.
  def addItem(item: Item): Either[OrderError, Order] =
    addItems(NEL(item))

  def addItems(items: NEL[Item]): Either[OrderError, Order] =
    status match {
      case x: Draft    => Right(copy(status= Draft(x.basket ++ items.toList)))
      case x: Checkout => Right(copy(status= Draft(x.basket.toList ++ items.toList)))
      case _           => Left(InvalidStatus(status))
    }

  // 1. Implement `checkout` which attempts to move the `Order` to "Checkout" status.
  // `checkout` requires the order to be in the "Draft" status, otherwise it returns an `InvalidStatus` error.
  // `checkout` requires the order to contain at least one item, otherwise it returns an `EmptyBasket` error.
  def checkout: Either[OrderError, Order] = {
    status match {
      case x: Draft =>
        NEL.fromList(x.basket) match {
          case None => Left(EmptyBasket)
          case Some(nel) => Right(copy(status= Checkout(nel, None)))
        }
      case _ => Left(InvalidStatus(status) )
    }
  }

  def updateDeliveryAddress(address: Address): Either[OrderError, Order] =
    status match {
      case x: Checkout =>
        val newStatus = x.copy(deliveryAddress= Some(address))
        Right(copy(status=newStatus))
      case _          => Left(InvalidStatus(status))
    }

  // 2. Implement `submit` which attempts to move the `Order` to "Submitted" status.
  // `submit` requires the order to be in the "Checkout" status and to have a delivery address.
  // If `submit` succeeds, the resulting order must be in "Submitted" status and
  // have the field `submittedAt` defined.
  // Note: You may need to extend `OrderError`
  def submit(now: Instant): Either[OrderError, Order] =
    status match {
      case x: Checkout =>
        x.deliveryAddress match {
          case None => Left(MissingDeliveryAddress)
          case Some(address) => Right(copy(status = Submitted(x.basket, address, submittedAt = now)))
        }
      case _ => Left(InvalidStatus(status))
    }

  // 3. Implement `deliver` which attempts to move the `Order` to "Delivered" status.
  // `deliver` requires the order to be in the "Submitted" status.
  // If `deliver` succeeds, the resulting order must be in "Delivered" status and
  // have the field `deliveredAt` defined.
  // If `deliver` succeeds, it also returns the time it took to deliver the order (duration
  // between `submittedAt` and `deliveredAt`).
  // Note: You may need to extend `OrderError`
  def deliver(now: Instant): Either[OrderError, (Order, Duration)] =
    status match {
      case x: Submitted =>
        val duration = Duration.between(x.submittedAt, now)
        Right((copy(status= Delivered(x.basket, deliveryAddress = x.deliveryAddress, x.submittedAt, deliveredAt=now)), duration))
      case _ => Left(InvalidStatus(status))
    }
}

object Order {
  // Creates an empty draft order.
  def empty(orderId: OrderId, now: Instant): Order =
    Order(
      id = orderId,
      status = Draft(Nil),
      createdAt = now
    )
}