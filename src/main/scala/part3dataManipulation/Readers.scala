package part3dataManipulation

object Readers {

  /*
    - configuration file => initial data structure
    - a DB layer
    - a HTTP layer
    - a business logic layer
   */
  case class Configuration(
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      nThreads: Int,
      emailReplyTo: String
  )

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatch" // select from db table and select status of the order
    def getLastOrderId(username: String) = 12345 // select max(orderId) from table where ...
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started ...") // this would start the actual server
  }

  val config = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "test@test.com")
  // cats Reader
  import cats.data.Reader
  // reads from the Configuration and returns a DbConnection
  // Reader is a specification of how we will get db connection from the configuration
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config)

  // Reader[I, O]
  val danielsOrdersStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(1))
  val danielsOrderStatus: String = danielsOrdersStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderId: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    // can also be done with for-comprehensions
    val userLastOrderIdFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

//    userLastOrderIdFor.run(config)
    usersLastOrderId.run(config)
  }

  /** Pattern:
    * 1. You create the initial data structure
    * 2. You create a reader which specifies how that data structure will be manipulated later
    * 3. You can then map and flatMap the reader to produce derived information
    * 4. When you need the final piece of information you call run on the reader with the initial data structure
    */

  // TODO:
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From $emailReplyTo; to $address >>> $contents"
  }

  // fetch the status of their last order
  // send them an email with the email service
  // Your last order has the status:

  def emailUser(username: String, userEmail: String) = {
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))
    val orderStatus = getLastOrderStatus(username)
    emailServiceReader
      .map(_.sendEmail(userEmail, s"Your last order has status $orderStatus"))
      .run(config)
  }

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("daniel", "bla@bla.com"))
  }
}
