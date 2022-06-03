package part5alien

import cats.Monoid

object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val cesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  /*
  How can we support cesar cypher logic for ints, doubles, Option[String]
   */

  implicit val doubleCrypto: Crypto[Double] = cesarCypher.imap(_.toString, _.toDouble)
  implicit val optionStringCrypto: Crypto[Option[String]] = cesarCypher.imap(_.getOrElse(""), Option(_))

  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  import cats.Invariant
  import cats.Show
  import cats.instances.string._
  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._
  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse(""))

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Let's encrypt")
    val decrypted = decrypt[String](encrypted)

    println(encrypted)
    println(decrypted)
    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))
    println(encrypt(Option("Let's encrypt")))
    println(decrypt[Option[String]](encrypted))
    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Math.PI)))
  }
}
