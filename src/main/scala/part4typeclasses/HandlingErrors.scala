package part4typeclasses

import cats.{ Applicative, Monad }

import java.util.concurrent.Executors
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(32) // Either[String, Int] = Right(32)
  val failure = monadErrorEither.raiseError[Int]("Something went wrong") // Either[String, Int] = Left("Something ...")

  // equivalent to recover with Try/Future
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 55
    case _         => 33
  }

  // recoverWith
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(55) // ErrorOr[Int]
    case _         => Left("Something else") // ErrorOr[Int]
  }

  // "filter"
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100)

  // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try], E = Throwable
  val exception = new RuntimeException("Really bad")
  val pureException = MonadError[Try, Throwable].raiseError(exception) // Failure[Exception]

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with Failure(exception)

  // applicatives => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]],
  import cats.ApplicativeError
  type ErrorsOr[T] = Validated[List[String], T]
  val applErrorValidated = ApplicativeError[ErrorsOr, List[String]]
  // pure, raiseError, handleError, handleErrorWith - same as monad error

  // extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError, handleErrorWith
  // requires the implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr]
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover { case _ =>
    43
  }

  import cats.syntax.monadError._
  val testedSuccess = success.ensure("Something bad")(_ > 100)
}
