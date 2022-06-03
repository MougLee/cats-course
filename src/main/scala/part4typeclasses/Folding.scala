package part4typeclasses

import cats.{ Eval, Monoid }
import part4typeclasses.Folding.ListExercises._

object Folding {

  // TODO - implement all in terms of foldLeft & foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B])((a, b) => f(a) :: b)
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List.empty[B])((b, a) => b ++ f(a))
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldRight(List.empty[A]) { (a, acc) =>
      if (predicate(a)) a :: acc else acc
    }

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldLeft(monoid.empty)(monoid.combine)
  }

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]
  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6
  import cats.instances.option._ // implicit Foldable[List]
  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  // fold right is stack safe because is using Eval regardless of the implementation of your container
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  // convenience methods
  import cats.instances.string._
  val anotherSum = Foldable[List].combineAll(List(1, 2, 3)) // implicit Monoid[Int]
  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // implicit Monoid[String]

  // nested data structures
  import cats.instances.vector._
  val intsNested = List(Vector(1, 2, 3), Vector(3, 4, 5))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  import cats.syntax.foldable._
  val sum3 = List(1, 2, 3).combineAll // requires implicit Foldable[List], Monoid[Int]
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)

  def main(args: Array[String]): Unit = {
    println(map(List(1, 2, 3, 4))(_ + 1))
    println(flatMap(List(1, 2, 3, 4))(a => List(a + 1)))
    println(filter(List(1, 2, 3, 4))(_ >= 3))

    import cats.instances.int._
    println(combineAll(List(1, 2, 3, 4, 5)))

  }
}
