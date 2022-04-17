package part1intro

object CatsIntro {

  // Eq
  // val aComparison = 2 == "a string" // will always fail, why does this even compile? it returns warning

  // part 1 - type class import
  import cats.Eq

  // part 2 - import type class instances for the types you need
  import cats.instances.int._

  // part 3 - type class API
  val intEquality = Eq[Int]
  // this code would not compile if both arguments do not have the same type
  val aTypeSafeComparison = intEquality.eqv(2, 3)

  // part 4 - use extension methods if applicable
  import cats.syntax.eq._
  val anotherTypeSafeComparison = 2 === 3 // false
  val notEqualComparison = 2 =!= 3 // true
  // val invalidComparison = 2 === "string" - doesn't compile

  // extension methods are only visible in the presence of the right type class instance

  // part 5 - extending the type class operations to composite type, e.g. lists
  import cats.instances.list._ // we bring Eq[List[Int]] in scope (int, because we already imported Eq[int]
  val aListComparison = List(1) === List(2)

  // part 6 - what if our type is not supported? --> Create type class instance for a custom type
  case class ToyCar(model: String, price: Double) // decision - two cars are equal if they have same price

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar]((t1, t2) => t1.price == t2.price)
  val compareTwoToyCars = ToyCar("Ferrari", 9.99) === ToyCar("Lamborgini", 9.99)
}
