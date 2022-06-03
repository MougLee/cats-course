package part3dataManipulation

object Writers {

  import cats.data.Writer
  // writer is a wrapper over a valuable value

  // point of the Writers
  // 1. define them at the beginning
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  // 2. manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, log stays the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) => // you have access to both values at the same time
    (logs :+ " found something interesting", value + 1)
  }

  // if we import a semigroup, the logs will be combined by their natural order
  import cats.instances.vector._ // imports Semigroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list._ // a Monoid[List] needed for the reset - reset needs Monoid of the logs type
  val emptyWriter = aWriter.reset

  // 3. dump either the value or the logs
  val desiredValue = aWriter.value // extract value (right side)
  val logs = aWriter.written // extract logs (left side)
  val (l, v) = aWriter.run

  // TODO: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit =
    if (n <= 0) println("Starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }

  def countAndLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector("Starting!"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))

  // Benefit #1: we work with pure FP (we are not printing the changes in the console)

  // TODO 2: rewrite naiveSum with writers
  def naiveSum(n: Int): Int =
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector.empty, 0)
    else
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithLogs(n - 1)
        _ <- Writer(Vector(s"Computed sum(${n - 1} = $lowerSum"), n)
      } yield n + lowerSum

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    println(countAndSay(10))
    println(naiveSum(100))
    sumWithLogs(100).written.foreach(println) // if we are using writers we can keep logs separate on multiple threads!
  }

}
