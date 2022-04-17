package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ ExecutionContext, Future }

object MonadTransformers {

  // operate with combination of monadic values
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List]

  // OptionT[List, Int] == List of options of int
  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // Either transformer
  import cats.data.EitherT
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("Something wrong"), Right(43), Right(2)))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))
  val futureOfEither2: EitherT[Future, String, Int] = EitherT(Future[Either[String, Int]](Right(45)))

  // TODO:
  // We have a multi machine cluster which will receive a traffic surge following a media appearance.
  // We measure bandwidth in units.
  // We want to allocate two of our servers to cope with the traffic spike
  // We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None    => EitherT.left(Future("Server not available"))
    case Some(b) => EitherT.right(Future(b))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      b1 <- getBandwidth(s1)
      b2 <- getBandwidth(s2)
    } yield b1 + b2 > 250

  // s1 and s2 can cope with the traffic spike
  // Left: why failure (server not exists, not enough bandwidth)
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = canWithstandSurge(s1, s2).transform {
    case Right(true)  => Right(s"$s1 and $s2 can cope with te traffic spike!")
    case Right(false) => Left(s"$s1 and $s2 do not have enough bandwidth!")
    case Left(v)      => Left(s"$s1 and $s2 can not cope with the incoming spike!")
  }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value) // no need to unwrap the option all the time
    val resultFuture = generateTrafficSpikeReport("server1.rockthejvm.com", "server2.rockthejvm.com").value
    resultFuture.foreach(println)
  }

}
