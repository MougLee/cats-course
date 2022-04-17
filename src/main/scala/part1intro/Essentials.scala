package part1intro

import java.util.concurrent.Executors
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

object Essentials {

  // values
  val aBoolean: Boolean = false

  // expressions
  val anIfExpression = if (2 > 3) "bigger" else "smaller"

  // instructions vs expressions
  // side effects - actions that do not evaluate to any meaningful value
  // and computer does something - e.g. prints something on the screen
  val theUnit = println("Hello, Scala")

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance, companion objects ...

  // generics
  class MyList[A]

  // method notation
  val three = 1 + 2 // infix method notations
  val anotherThree = 1.+(2)

  // functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(45) // 46

  // we can also pass it as a higher order function - most typical methods map, flatMap, filter ...
  val processedList = List(1, 2, 3).map(incrementer) // List(2,3,4)

  // for-comprehension

  // options and try
  val anOption = Option( /* Something that might be null */ 3)
  val anAttempt = Try( /* Something that might throw */ 42) // Success(42) or Failure
  val aModifiedAttempt: Try[Int] = anAttempt.map(_ * 2)

  // pattern matching

  // Futures - their values are computed on some other thread at some point
  // in scala 2.13 the execution of global execution context was changes
  // to save some Future spawning overhead -> nested futures will not run
  // in parallel unless we use the blocking API
  // we will use our own EC
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future {
    // a bit of code that will be evaluated on another thread
    42
  }

  // wait for completion (async)
  // onComplete receives Try -> use pattern matching
  aFuture.onComplete {
    case Success(value)     => println(s"Async meaning of life is $value")
    case Failure(exception) => println(s"Meaning of life failed: $exception")
  }

  // map a Future
  val anotherFuture = aFuture.map(_ + 1) // Future(43) when it completes - onComplete is also executed on another thread

  // partial functions
  // difference between partial functions and regular functions
  // is that partial function only receives an argument that satisfies a specific pattern
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1   => 43
    case 8   => 56
    case 100 => 999
  }

  // some more advanced stuff
  trait HigherKindredType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def isSequential = true
  }

  def main(args: Array[String]): Unit = {}

}
