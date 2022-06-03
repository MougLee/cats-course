package part3dataManipulation

object Evaluation {

  /*
   Cats makes a distinction between
   - evaluating an expression eagerly
   - evaluating lazily and every time you request it
   - evaluating lazily and keeping the value (memoizing)
   */

  // Eval is wrapper around a value
  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now")
    52
  }

  val redoEval: Eval[Int] = Eval.always {
    println("Computing again")
    42
  }

  val delayEval: Eval[Int] = Eval.later {
    println("Computing later")
    632
  }

  val composedEvaluation = instantEval.flatMap(val1 => delayEval.map(val2 => val1 + val2))
  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- delayEval
  } yield value1 + value2 // same as above

  // TODO 1:
  val evalEx1 = for {
    a <- delayEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // remember a computed value
  val dontRecompute = redoEval.memoize

  val tutorial = Eval
    .always {
      println("Step 1")
      "Put the guitar in your lap"
    }
    .map { step1 =>
      println("Step 2")
      s"$step1 then put your left hand on the neck"
    }
    .memoize // remember the value up to this point
    .map { step1And2 =>
      println("More complicated")
      s"$step1And2 then with the right hand strike the strings"
    } // this map (step) will be evaluated every time

  // TODO 2: implement defer such that defer(Eval.now) does NOT run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  // TODO 3: rewrite the method with evals instead of regular values
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    // TRICK! By wrapping in defer we avoid stack overflowing (we have a chain of Eval.later
    // because chain of evals is evaluated in a tail recursive --> won't stack overflow
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    // evaluated eagerly before you even use the value
    println(instantEval.value)
    println(redoEval.value)
    println(delayEval.value)
    println(delayEval.value) // expression not evaluated again

    println(composedEvaluation.value)

    println(evalEx1.value)
    println(evalEx1.value)
    println(dontRecompute)

    println(tutorial.value)
    println(tutorial.value)

    val a = defer(Eval.now {
      println("Now!")
      42
    })

    println(a.value)
  }
}
