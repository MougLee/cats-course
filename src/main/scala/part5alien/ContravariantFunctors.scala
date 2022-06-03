package part5alien

import cats.Monoid

object ContravariantFunctors {

  // not related to variance in generics

  trait Format[T] { self => // contravariant type classes -> typeclass that has a contramap method
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit format: Format[A]) = format.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // problem: given Format[MyTYpe], can we also have a Format[Option[MyType]]
//  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = new Format[Option[T]] {
//    override def format(value: Option[T]): String = f.format(value.get)
//  }
  // ==>
  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

  // Contramap applies transformation in REVERSE ORDER

  import cats.Contravariant
  import cats.Show // similar to our Format typeclass
  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Nothing weird so far"))
    println(format(42))
    println(format(true))
    println(format(Option(42)))
  }
}
