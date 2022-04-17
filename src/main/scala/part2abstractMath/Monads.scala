package part2abstractMath

import java.util.concurrent.{ ExecutorService, Executors }
import scala.concurrent.{ ExecutionContext, Future }

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List("a", "b", "c")

  // TODO 1.1: how do you create all combinations of (number, char)?
  val combo = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  // options
  val numberOption = Option(2)
  val charOption = Option('d')
  val comboOption = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // same for Future

  /*
    Pattern:
    - wrapping a value into a monadic value
    - the flatMap mechanism (flatMap is NOT ITERATION!) FLAT MAP IS A GENERAL TRANSFORMATION PATTERN!!

    MONADS
   */

  trait MyMonad[M[_]] {
    def pure[A](a: A): M[A] // wrap a value

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  import cats.Monad
  import cats.instances.option._
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(a => Some(a + 1))

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4, 5)

  // TODO: use a Monad of a Future
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(42)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1))

  // specialised API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  // generalised
  def getPairs[M[_], A, B](a: M[A], b: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(a)(ma => monad.map(b)(mb => (ma, mb)))

  // extension methods - weirder imports - pure, flatMap
  import cats.syntax.applicative._ // pure is here
  import cats.syntax.flatMap._ // flatMap is here
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used
  val oneList = 1.pure[List]

  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement the map method in the MyMonad trait
  // Monads extend functors
  val oneOptionMapped2 = Monad[Option].map(Option(2))(_ + 1) // call map explicitly
  import cats.syntax.functor._ // map is here
  val oneOptionMapped = oneOption.map(_ + 1) // use extension method - or original map in this case

  // map + flatMap --> for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  def getPairs2[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  def getPairs3[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  def main(args: Array[String]): Unit = {
    println(getPairs(numbersList, charsList))
    println(getPairs2(numbersList, charsList))
  }
}
