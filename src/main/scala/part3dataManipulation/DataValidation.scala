package part3dataManipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataValidation {

  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(42) // like "right" in the Either
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value

  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "This is the error")

  def testPrime(n: Int): Boolean = {
    @tailrec
    def tailrecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailrecPrime(Math.abs(n / 2))
  }

  // TODO: use Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isToBig: List[String] = if (n <= 100) List() else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if (testPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isToBig ++ isNotPrime)
  }

  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated
      .cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be less than or equal to 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)

  // test a valid value
  aValidValue.ensure(List("Something went wrong"))(_ % 2 == 0)

  // transform a value
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  // interop with stdLib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))

  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2: a form validation
  object FormValidation {
    import cats.instances.string._
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName is required."))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must not be blank."))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid."))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Email must be at least 10 chars long."))

    /*
      fields are name, email, password
      rules are:
       - all fields required
       - name must  not be blank
       - email must have @ char
       - passwords must have at least 10 chars
     */
    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "name")
        .andThen(name => nonBlank(name, "name"))
        .combine(getValue(form, "email").andThen(emailProperForm))
        .combine(getValue(form, "password").andThen(passwordCheck))
        .map(_ => "User registration complete")
  }

  import cats.syntax.validated._
  // left type is error type, desired value is Int type (right)
  val meaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError = "Something went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    val form = Map(
      "name" -> "Matic",
      "email" -> "test@test",
      "password" -> "password1234"
    )

    println(FormValidation.validateForm(form))
  }
}
