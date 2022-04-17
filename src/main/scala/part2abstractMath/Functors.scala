package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1)
  val aModifiedOption = Option(2).map(_ + 1)
  val aModifiedTry = Try(42).map(_ + 1)

  // F[_] --> MyFunctor type class excepts a parameter that itself is a generic
  // this is a simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1)

  import cats.instances.try_._ // watch out, _ in the name try_
  val incrementedTry = Functor[Try].map(Try(4))(_ + 1)

  // functor becomes important when we want to generalise an API
  def do10XList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10XOption(list: Option[Int]): Option[Int] = list.map(_ * 10)
  def do10XTry(list: Try[Int]): Try[Int] = list.map(_ * 10)

  // def do10X[F[_]](list: F[Int]): F[Int] = list.map(_ * 10) --> we can't enforce that F[Int] has a MAP method
  // --> use functor
  def do10X[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // TODO 1: define functor for a binary tree
  trait Tree[+T]
  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object treeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      case Leaf(value)                => Leaf(f(value))
    }
  }

  // extension method - can use map
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)

  // TODO 2: write shorted do10x method by using the extension methods
  def do10X2[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10X(List(1, 2, 3)))
    println(do10X(Option(1)))
    println(do10X(Try(1)))

    val tree = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
    println(tree)
    println(treeFunctor.map(tree)(_ + 1))
    println(do10X[Tree](tree)) // we need to add the type argument to help compiler figure out Branch is a Tree
    // no need for the type argument with smart constructors! It is always returning a tree
    println(do10X(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
  }

}
