package part1intro

object Implicits {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  // implicit class needs to have constructor with one param
  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  // explicitly calling the method of the implicit class
  val imporsonableString = new ImpersonableString("Peter")
  imporsonableString.greet

  // the goal of the implicit class is to be able to wrap the parameter in the
  // implicit class automatically e.g.
  val greeting = "Peter".greet

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultValue = 10
  val incremented2 = increment(2)

  def multiply(x: Int)(implicit times: Int) = x * times
  val times = multiply(2)

  // more complex example
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(serializer.toJson).mkString("[", ",", "]")

  implicit val personSerializer = new JsonSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name": "  ${person.name}"}
         |""".stripMargin
  }
  val personsJson = listToJson(List(Person("Alice"), Person("Bob")))
  // implicit argument is used to PROVE THE EXISTENCE of a type

  // implicit methods
  // all case classes extend Product --> this is an implicit def for all T types that are a subtype of Product
  implicit def oneArgCaseClassSerializer[T <: Product]: JsonSerializer[T] = new JsonSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}": "${value.productElement(0)}"}
         |""".stripMargin
  }
  // --> we don't need to implement implicit json serializers for each type any more

  case class Cat(name: String)
  listToJson(List(Cat("Bu"), Cat("Pax")))
  // implicit methods are used to PROVE THE EXISTENCE of a type
  // implicit methods can be used for conversions (DISCOURAGED)

  List(1, 2, 3).sorted

  def main(args: Array[String]): Unit = {}

}
