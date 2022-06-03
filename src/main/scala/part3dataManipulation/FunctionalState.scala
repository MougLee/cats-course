package part3dataManipulation

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(cnt => ((cnt + 1), s"Counted $cnt"))

  val (eleven, counted10) = countAndSay.run(10).value
  // state = "iterative" computations

  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained ${a + 1}"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }

  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { cart =>
    (ShoppingCart(item :: cart.items, cart.total + price), cart.total + price)
  }

  val myCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("Item 1", 9)
    _ <- addToCart("Item 2", 19)
    total <- addToCart("Item 3", 1)
  } yield total

  // TODO 2: pure mental gymnastics
  // returns a State data structure such that, when run, it will not change the state but will issue the value of f(a)
  def inspect[A, B](f: A => B): State[A, B] = State((a: A) => (a, f(a)))
  // returns a State data structure such that, when run, returns the value of the state and makes no changes
  def get[A]: State[A, A] = State((a: A) => (a, a))
  // returns a State data structure such that, when run, returns a Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State((_: A) => (value, ()))
  // returns a State data structure such that, when run, returns a Unit and sets the state to f(state)
  def modify[A](value: A, f: A => A): State[A, Unit] = State(value => (f(value), ()))

  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(compositeTransformation2.run(10).value)

    println(myCart.run(ShoppingCart(Nil, 0)).value)
  }
}
