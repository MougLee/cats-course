package part4typeclasses

import cats.{ Applicative, Foldable, Functor, Monad }

import java.util.concurrent.Executors
import scala.concurrent.{ ExecutionContext, Future }

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val servers: List[String] = List("server-test", "server-staging", "server-prd")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  /*
  We have:
  - a List[String]
  - a func String => Future[Int]
  we want a Future[List[Int]]
   */
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (acc, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- acc
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  // much more elegant solution
  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // TODO 1
  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  import cats.syntax.apply._ // mapN
  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wrapperAcc, el) =>
      val wElement: F[B] = func(el)
      (wrapperAcc, wElement).mapN(_ :+ _)
    }

  // TODO 2
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    list.foldLeft(List.empty[A].pure[F])((wrappedAcc, el) => (wrappedAcc, el).mapN(_ :+ _))

  def listSequence2[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  // TODO 3
  import cats.instances.vector._
  val allPairs = listSequence(
    List(Vector(1, 2), Vector(3, 4))
  ) // Vector[List[Int] - all the possible 2-tuples (1,3)(1,4)(2,3)(2,4)
  listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

  // TODO 4 - what is the result
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))

  filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2,4,6))
  filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List] => Applicative[ErrorsOr]
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"Predicate for $n failed"))
    }

  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2,4,6))
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0) // Invalid(List(Predicate for 1, for 3))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    import cats.Id
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]
  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse._ // sequence + traverse
  val allBandwidthsCats2 = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTrueValidated)
    println(someFalseValidated)
  }
}
