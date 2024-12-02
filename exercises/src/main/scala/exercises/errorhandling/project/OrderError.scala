package exercises.errorhandling.project

sealed trait OrderError
object OrderError {
  case object EmptyBasket                         extends OrderError
  case object MissingDeliveryAddress              extends OrderError
  case object MissingSubmittedAt                     extends OrderError
  case class InvalidStatus(currentStatus: OrderStatus) extends OrderError

}