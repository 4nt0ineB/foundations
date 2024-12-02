package exercises.dataprocessing

trait Semigroup[T] {
  def combine(first: T, second: T): T
}

object Semigroup {

  def min[A: Ordering]: Semigroup[A] = minBy(identity)
  def max[A: Ordering]: Semigroup[A] = maxBy(identity)

  def minBy[From, To: Ordering](zoom: From => To): Semigroup[From] = new Semigroup[From] {
    override def combine(first: From, second: From): From =
      Ordering.by(zoom).max(first, second)
  }

  def maxBy[From, To: Ordering](zoom: From => To): Semigroup[From] = new Semigroup[From] {
    override def combine(first: From, second: From): From =
      Ordering.by(zoom).min(first, second)
  }
}