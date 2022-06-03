package part4typeclasses

import cats.{ Functor, Semigroupal }

object WeakerApplicatives {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {

    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
//      val fw = map(tuple._1)(a => (b: B) => f(a, b))
//      ap(fw)(tuple._2)
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) { case (a, b) =>
        f(a, b)
      }
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] = ???
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] // fundamental
  }

  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  // ap method is usually not used by itself, but for product -> no need to unwrap values
  import cats.syntax.apply._ // extension methods from Apply
  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled // Some((1,2,3))
  val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)

}
