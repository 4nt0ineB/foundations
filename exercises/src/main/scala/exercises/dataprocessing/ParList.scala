package exercises.dataprocessing

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]], ec: ExecutionContext) {
  def toList: List[A] = partitions.flatten
  def map[To](update: A => To): ParList[To] = copy(partitions= partitions.map(part => part.map(update)))

  def reduceMap[To](zoom: A => To)(semigroup: Semigroup[To]): Option[To] = {
    partitions.filter(_.nonEmpty) match {
      case Nil => None
      case partitions =>
        val reducedPartitions = partitions.map(_.map(zoom).reduceLeft(semigroup.combine))
        val reducedAll = reducedPartitions.reduceLeft(semigroup.combine)
        Some(reducedAll)
    }
  }

  def size(samples: ParList[Sample]): Int =
    samples.parFoldMap(_ => 1)(Monoid.sum[Int])

  // e. Implement `monoFoldLeft`, a version of `foldLeft` that does not change the element type.
  // Then move `monoFoldLeft` inside  the class `ParList`.
  // `monoFoldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->       x   (folded partition 1)  \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->       y   (folded partition 2) - z (final result)
  // Partition 3:                          Nil -> default (partition 3 is empty)  /
  def monoFoldLeft(param: Monoid[A]) =
    partitions
      .map(_.foldLeft(param.default)(param.combine))
      .foldLeft(param.default)(param.combine)

  def foldMap[To](update: A => To)(monoid: Monoid[To]): To =
    partitions.map {
      partition =>
        partition.foldLeft(monoid.default)((state: To, value: A) => monoid.combine(state, update(value)))
    }.foldLeft(monoid.default)(monoid.combine)

  def parFoldMap[To](update: A => To)(monoid: Monoid[To]): To = {
    def foldPartition(part: List[A]): Future[To] = {
      Future {
        part.foldLeft(monoid.default)((state: To, value: A) => monoid.combine(state, update(value)))
      }(ec)
    }
    partitions                                  // List[List[A]]
      .map(foldPartition)                       // List[Future[To]]
      .map(Await.result(_, Duration.Inf))       //. List[To]
      .foldLeft(monoid.default)(monoid.combine) // To
  }

  def shuffle: ParList[A] = {
    val shuffledList = Random.shuffle(toList)
    val partitionSize = if (partitions.isEmpty) 1 else partitions.head.size
    ParList.byPartitionSize(partitionSize, shuffledList, ec)
  }

}

object ParList {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](ec: ExecutionContext, partitions: List[A]*): ParList[A] =
    ParList(partitions.toList, ec)

  // Creates a ParList by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParList(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A](partitionSize: Int, items: List[A], ec: ExecutionContext): ParList[A] =
    if (items.isEmpty) ParList(ec)
    else ParList(items.grouped(partitionSize).toList, ec)

}