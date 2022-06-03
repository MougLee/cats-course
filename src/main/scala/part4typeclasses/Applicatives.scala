package part4typeclasses

object Applicatives {

  // Applicatives = Functor + the pure monad
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // implicit Applicative[Option]
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // Some(2)

  // applicatives are useful where Functor is useful -> generalizing APIs + you can wrap a normal value

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List] // List(2)
  // pure same method as with Monad - Monad extends Applicatives and uses the pure method from them
  val aSweetOption = 2.pure[Option] // Some(2)

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  // Validated is a candidate to be an Applicable, since the valid() method wraps the regular value into a Validated type
  // --> exactly the same as what a pure() method does
  val aValidated: ErrorsOr[Int] = Validated.valid(3) // "pure"
  val aModifiedValidated: ErrorsOr[Int] = aValidated.map(_ + 1) // map
  // validated is not a monad (associativity law does not hold) and applicative is the strongest type class
  val validatedApplicative = Applicative[ErrorsOr]

  // TODO: thought experiment
  // def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T] = ??? // already implemented inside applicative
  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  // Applicatives have this ap method def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
  // Applicatives can implement product from Semigroupal --> Applicative extends Semigroupal

  def main(args: Array[String]): Unit = {}

}
