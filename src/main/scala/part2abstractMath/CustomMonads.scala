package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    // looping function that will run f until we get a desired value
    // it must be tail recursive so it doesn't stack overflow
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None           => None
      case Some(Left(v))  => tailRecM(v)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  // TODO: define Monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v)  => tailRecM(v)(f)
      case Right(v) => v
    }
  }

  // harder example
  sealed trait Tree[+A]
  final case class Leaf[+A](a: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // TODO: define a Monad for this tree
  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(a)             => f(a)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v))       => stackRec(f(v))
        case Leaf(Right(v))      => Leaf(v)
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }

      def tailRec(todo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] =
        if (todo.isEmpty) done.head
        else
          todo.head match {
            case Leaf(Left(v))  => tailRec(f(v) :: todo.tail, expanded, done)
            case Leaf(Right(v)) => tailRec(todo.tail, expanded, Leaf(v) :: done)
            case node @ Branch(left, right) =>
              if (!expanded.contains(node)) tailRec(right :: left :: todo, expanded + node, done)
              else {
                val newLeft = done.head
                val newRight = done.tail.head
                val newBranch = Branch(newLeft, newRight)
                tailRec(todo.tail, expanded, newBranch :: done.drop(2))
              }
          }
      // stackRec(f(a))
      tailRec(List(f(a)), Set.empty, List.empty)
    }
  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(10), Leaf(20))
    val changeTree = TreeMonad.flatMap(tree)(t => Branch(Leaf(t + 1), Leaf(t + 2)))
    println(changeTree)
  }

}
