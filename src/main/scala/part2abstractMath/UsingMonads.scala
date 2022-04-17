package part2abstractMath

import scala.util.{ Failure, Success, Try }

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List]
  val aSimpleList = monadList.pure(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // above pattern is applicable to Option, Try, Future, ... and many other types

  // either is also a monad
  // any time a left (undesirable) value has a concrete type,
  // we can define it as a new type with only one generic value and use it as a Monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading =
    loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Error, loading ..."))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(status: OrderStatus): LoadingOr[String] =
    if (status.orderId > 1000) Left("Not available yet, refreshing data") else Right("Netherlands")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  // if we want to use for-comprehension we need to use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]: Monad](service: HttpService[M], payload: String): M[String] =
    for {
      conn <- service.getConnection(config)
      res <- service.issueRequest(conn, payload)
    } yield res

  // TODO: provide an implementation of the interface above
  object HttpServiceTry extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] = {
      val conn = for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

      conn.map(c => Success(c)).getOrElse(Failure(new RuntimeException("Invalid configuration")))
    }

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      if (payload.length <= 20) Success(s"Request ($payload) has been accepted.")
      else Failure(new RuntimeException("Payload too long."))
  }

  val responseTry = HttpServiceTry
    .getConnection(config)
    .flatMap(conn => HttpServiceTry.issueRequest(conn, "Hello!"))

  val responseTryFor = for {
    conn <- HttpServiceTry.getConnection(config)
    response <- HttpServiceTry.issueRequest(conn, "Hello!")
  } yield response

  // TODO 4: implement another HttpService using LoadingOr or ErrorOr

  object EitherOrHttpService extends HttpService[ErrorOr] {

    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port")) Left(new RuntimeException("Invalid config"))
      else Right(Connection(cfg("host"), cfg("port")))

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length <= 20) Right(s"Request ($payload) has been accepted.")
      else Left(new RuntimeException("Payload too long."))
  }

  def main(args: Array[String]): Unit = {
    println(responseTry)
    println(responseTryFor)
    println(getResponse(EitherOrHttpService, "Hello, monads!"))
  }

}
