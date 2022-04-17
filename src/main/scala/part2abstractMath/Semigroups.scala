package part2abstractMath

object Semigroups {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "Cats!") // concatenation

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // TODO 1: support new type
  case class Expense(id: Long, amount: Double)

  // create instance of semigroup for Expense
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (x, y) =>
    Expense(Math.max(x.id, y.id), x.amount + y.amount)
  }

  // extension methods from semigroups - |+|
  import cats.syntax.semigroup._
  val intSum = 2 |+| 3 // requires the presence of implicit Semigroup[Int]
  val stringConcat = "We " |+| "like semigroups"

  // TODO 2: implement reduceThings2 with the combination function
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)

  // we can simplify the function and remove the (implicit semigroup: ...) part by using the TYPE CONTEXT
  // [T: Semigroup] basically ads the implicit param
  def reduceThings3[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    val numbers = (1 to 10).toList
    println(reduceInts(numbers))

    val strings = List("I am ", "starting ", "to like", "Semigroup")
    println(reduceStrings(strings))

    println(reduceThings(numbers))
    println(reduceThings(strings))

    import cats.instances.option._
    val numberOptions = numbers.map(Option(_))
    println(reduceThings(numberOptions))

    val expenses = List(Expense(1, 20), Expense(2, 9))
    println(reduceThings(expenses))
  }

}
