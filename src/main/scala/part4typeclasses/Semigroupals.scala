package part4typeclasses

import java.util.concurrent.{ ExecutorService, Executors }
import scala.concurrent.{ ExecutionContext, Future }

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](a: F[A], b: F[B]): F[(A, B)] // creates a tuple of two values
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future(1), Future(2))

  import cats.instances.list._
  val listTupled = Semigroupal[List].product(List(1, 2), List("a", "b"))

  // TODO: implement
  import cats.Monad
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  // nicer implementation with use of cats
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  def productWithMonads2[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // Monads EXTEND SEMIGROUPALS - monad can do products

  // Monads need to follow monad laws, but we don't necessarily
  // want to stick to the laws when we are using e.g. Validated --> Semigroupal
  // example
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]
  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombo = eitherSemigroupal.product( // implemented in terms of map/flatMap --> the errors are not propagated
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This can't be right"))
  )

  // TODO 2: Define Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(invalidsCombination)
    println(eitherCombo)
  }
}
