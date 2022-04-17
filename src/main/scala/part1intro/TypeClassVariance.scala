package part1intro

object TypeClassVariance {

  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)

  // val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found

  // variance
  class Animal

  class Cat extends Animal

  // covariant type: subtype is propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to the generic type
  // I want a vet of cat --> I will return a "better" type - a vet of all animals
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, Vet[Animal] <: Vet[Cat]

  // rule of thumb: HAS a T = covariant, if it ACTS on T = contravariant
  // variance affect how TC instances are being fetched

  // contravariant type class
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow") // implementation not important

  makeSound[Animal] // OK, compiles - TC instance defined above
  makeSound[Cat] // also compiles, because TC instance for Animal is also applicable for Cats -->
  // sound maker for animals can make sound for cat as well
  // rule 1: contravariant type classes can use the superclass instance if nothing else is available strictly for that type

  // if Eq would be contravariant we would be able to do Some(2) === None -> use subtype of option, but is is not

  // covariant type class
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "Animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats!"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show
  // rule 2: covariant type classes will always use the more specific TC instance for that type, but
  // may confuse the compile if the general TC is also present

  // rule 3: tradeoff to pick
  // either make your general TC available for subtypes as well (contravariance)
  // or have the benefit of picking the most specific TC (covariant)
  // but you can't have both!
  // Cats uses INVARIANT type classes -->
  // use the general type with smart constructors
  Option(2) === Option.empty[Int]

  def main(args: Array[String]): Unit =
    println(organizeShow[Cat]) // ok, compile will inject CatsShow as implicit
  // println(organizeShow[Animal]) // will not compile - two potential instances of TC (Cats and Animal)

}
