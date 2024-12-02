package exercises.dataprocessing

case class Summary(
  min: Option[Sample], // Sample with lowest temperature
  max: Option[Sample], // Sample with highest temperature
  sum: Double, // sum of all temperatures in Fahrenheit
  size: Int // number of Samples
) {

  def average: Option[Double] =
    Option.unless(size == 0)(sum / size)

  override def toString: String =
    f"Summary(avg = ${average.getOrElse(0.0)}%.2f, " +
      s"size = $size,\n  " +
      s"min = $min,\n  " +
      s"max = $max\n)"
}

object Summary {
  val monoid: Monoid[Summary] = new Monoid[Summary] {
    override def default: Summary = Summary(min = None, max = None, sum = 0, size = 0)

    override def combine(first: Summary, second: Summary): Summary = {
      Summary(
        min = Monoid.minBy((sample: Sample) => sample.temperatureFahrenheit).combine(first.min, second.min),
        max = Monoid.maxBy((sample: Sample) => sample.temperatureFahrenheit).combine(first.max, second.max),
        sum = Monoid.sum[Double].combine(first.sum, second.sum),
        size = Monoid.sum[Int].combine(first.size, second.size)
      )
    }
  }
}