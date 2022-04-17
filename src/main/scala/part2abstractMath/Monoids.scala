package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import |+| extension method

  val numbers = (1 to 1000).toList
  // |+| is associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  //  def combineFold[T: Semigroup](list: List[T]): T = list.foldLeft( /* WHAT VALUE DO I PUT HERE? */ )(_ |+| _)
  // semigroup is not enough - we need a "zero/empty" value --> we need a MONOID

  // Monoid is the same as Semigroup except that is also provides the "zero" value
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(2, 4)
  val zero = intMonoid.empty // 0 for int

  import cats.instances.string._
  val emptyString = Monoid[String].empty // "" for string
  val combineString = Monoid[String].combine("I undersand ", "Monoids")

  import cats.instances.option._
  val emptyOption = Monoid[Option[Int]].empty
  val combineOption = Monoid[Option[Int]].combine(Option(2), None) // Some(2)

  // extension methods for Monoid - |+| --> we can use one form Semigroup or from Monoid
  val combinedOptionFancy = Option(3) |+| Option(2)

  // TODO: implement a combineFold
  def combineFold[T: Monoid](list: List[T]): T = list.foldLeft(Monoid[T].empty)(_ |+| _)
  // OR
//  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldLeft(monoid.empty)(_ |+| _)

  // TODO: combine a list of phonebooks as Maps[String, Int]
  val phonebooks = List(
    Map("Alice" -> 235, "Bob" -> 647),
    Map("Charlie" -> 372, "Daniel" -> 889),
    Map("Tina" -> 123)
  )

  import cats.instances.map._

  // TODO 3
  case class ShoppingCart(items: List[String], total: Double)
  implicit val shoppingCartMonoid = Monoid
    .instance[ShoppingCart](ShoppingCart(Nil, 0.0), (a, b) => ShoppingCart(a.items ++ b.items, a.total + b.total))

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)

    println(combineFold(numbers))
    println(combineFold(List("I ", "like ", "Monoids")))
    println(combineFold(phonebooks))

    println(checkout(List(ShoppingCart(List("Phone"), 99), ShoppingCart(List("Robot"), 99))))
  }

}
