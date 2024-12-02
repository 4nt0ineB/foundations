package exercises.dataprocessing

trait Monoid[A] {
  def default: A
  def combine(first: A, second: A): A

}

object Monoid {


  val sumDoubleInt: Monoid[(Double, Int)] = zip(Monoid.sum[Double], Monoid.sum[Int])
  val maxSample: Monoid[Option[Sample]] = maxBy((sample: Sample) => sample.temperatureFahrenheit)
  val minSample: Monoid[Option[Sample]] = minBy((sample: Sample)=> sample.temperatureFahrenheit)


  def sum[A](implicit numeric: Numeric[A]) : Monoid[A] = new Monoid[A] {
    override def default: A = numeric.zero

    override def combine(first: A, second: A): A = numeric.plus(first,second)
  }

  def zip[A, B](monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
    override def default: (A, B) = (monoidA.default, monoidB.default)

    override def combine(first: (A, B), second: (A, B)): (A, B) =
      (
        monoidA.combine(first._1, second._1),
        monoidB.combine(first._2, second._2)
      )
  }

  private def compare[From](compare: (From, From) => From): Monoid[Option[From]] = new Monoid[Option[From]] {
    override def default: Option[From] = None

    override def combine(first: Option[From], second: Option[From]): Option[From] = {
      (first, second) match {
        case (None, None) => None
        case (Some(sample), None) => Some(sample)
        case (None, Some(sample)) => Some(sample)
        case (Some(sample1), Some(sample2)) => Some(compare(sample1, sample2))
      }
    }
  }

  def maxBy[From, To: Numeric](zoom: From => To): Monoid[Option[From]] = compare { (a, b) =>
    val numeric = implicitly[Numeric[To]]
    if(numeric.gt(zoom(a), zoom(b))) a else b
  }

  def minBy[From, To: Numeric](zoom: From => To): Monoid[Option[From]] = compare { (a, b) =>
    val numeric = implicitly[Numeric[To]]
    if(numeric.lt(zoom(a), zoom(b))) a else b
  }



}

//f(x, 0) = x
//f(0, x) = x
//f(f(x, y), z) = f(x, f(y,z))