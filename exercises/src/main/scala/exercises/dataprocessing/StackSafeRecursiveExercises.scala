package exercises.dataprocessing

import java.util.Optional
import scala.annotation.tailrec

object StackSafeRecursiveExercises {

  def unsafeSum(numbers: List[Int]): Int =
    numbers match {
      case Nil          => 0
      case head :: tail => head + unsafeSum(tail)
    }

  def sum(numbers: List[Int]): Int = {
    @tailrec
    def go(numbers: List[Int], accumulator: Int): Int =
      numbers match {
        case Nil          => accumulator
        case head :: tail => go(tail, accumulator + head)
      }
    go(numbers, 0)
  }

  // a. Implement `min` using a recursion
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  def min(numbers: List[Int]): Option[Int] = {
    @tailrec
    def go(numbers: List[Int], currentMin: Option[Int]): Option[Int] = {
      numbers match {
        case Nil => currentMin
        case head::tail => go(tail, Some(head min currentMin.getOrElse(head)))
      }
    }
    go(numbers, None)
  }

  // b. Implement `reverse` using a recursion
  // such as reverse(List(2,5,1,8)) == List(8,1,5,2)
  // and     reverse(Nil) == Nil
  // Note: Ensure size is stack-safe
  def reverse[A](items: List[A]): List[A] = {
    @tailrec
    def go(items: List[A], reversed: List[A]): List[A] = {
      items match {
        case head :: next => go(next, head +: reversed)
        case Nil => reversed
      }
    }

    go(items, List())
  }

  // c. Implement `foldLeft` using a recursion
  // Note: Ensure size is stack-safe
  def foldLeft[From, To](items: List[From], default: To)(combine: (To, From) => To): To = {
    @tailrec
    def go(items: List[From], acc: To): To = {
      items match {
        case head :: next => go(next, combine(acc, head))
        case Nil => acc
      }
    }
    go(items, default)
  }

}
