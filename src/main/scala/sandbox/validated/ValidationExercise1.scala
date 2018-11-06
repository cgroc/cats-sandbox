package sandbox.validated

import cats.ApplicativeError
import cats.data.Validated
import cats.instances.string._
import cats.syntax.apply._// for mapN and other nice syntax

/*
 * IntelliJ can't handle the imports and the mapN, this is because the presentation compiler has issues
 * with the type constructors. You _can_ get around this by providing a type alias - nuts!
 */

case class User(name: String, age: Int, email: String)

object EitherCode {

  // Check if the name is empty
  def validateName(name: String): Either[String, String] =
    if(name.isEmpty)
      Left("Can't have an empty name!")
    else
      Right(name)

  // Check if age > 0
  def validateAge(age: Int): Either[String, Int] =
    if(age < 0)
      Left("Can't have a negative age!")
    else
      Right(age)

  // Check if email contains @
  def validateEmail(email: String): Either[String, String] =
    if(email.contains("@"))
      Right(email)
    else
      Left("Not a valid email address")

  // validate all 3 fields, assemble a user
  def validateUser(name: String, age: Int, email: String): Either[String, User] =
    for {
      n <- validateName(name)
      a <- validateAge(age)
      e <- validateEmail(email)
    } yield User(n, a, e)

}

object ValidatedCode {

  // Check if the name is empty
  def validateName(name: String): Validated[String, String] =
    if(name.isEmpty)
      Validated.Invalid("Can't have an empty name!")
    else
      Validated.Valid(name)

  // Check if age > 0
  def validateAge(age: Int): Validated[String, Int] =
    if(age < 0)
      Validated.Invalid("Can't have a negative age!")
    else
      Validated.Valid(age)

  // Check if email contains @
  def validateEmail(email: String): Validated[String, String] =
    if(email.contains("@"))
      Validated.Valid(email)
    else
      Validated.Invalid("Not a valid email address")

  // validate all 3 fields, assemble a user
  def validateUser(name: String, age: Int, email: String): Validated[String, User] =
    (
      validateName(name),
      validateAge(age),
      validateEmail(email)
    ).mapN(User.apply)

}

class GenericValidation[F[_]](implicit F: ApplicativeError[F, String]) {

  def validateName(name: String): F[String] =
    if(name.isEmpty)
      F.raiseError("Can't have an empty name!")
    else
      F.pure(name)

  // Check if age > 0
  def validateAge(age: Int): F[Int] =
    if(age < 0)
      F.raiseError("Can't have a negative age!")
    else
      F.pure(age)

  // Check if email contains @
  def validateEmail(email: String): F[String] =
    if(email.contains("@"))
      F.pure(email)
    else
      F.raiseError("Not a valid email address")

  // validate all 3 fields, assemble a user
  def validateUser(name: String, age: Int, email: String): F[User] =
    (
      validateName(name),
      validateAge(age),
      validateEmail(email)
    ).mapN(User.apply)
}

object ValidatedApp extends App {

  type UserValidation[A] = Validated[String, A]

  val validation = new GenericValidation[UserValidation]

  val u1 = validation.validateUser("Bob", 29, "bob@bob.com")

  println(u1)

  val u2 = validation.validateUser("Bob", -2, "bob@bob.com")

  println(u2)

  val u3 = validation.validateUser("Bob", -2, "bobrules!")

  println(u3)

  import cats.instances.either._

  type EitherValidation[A] = Either[String, A]

  val eitherValidation = new GenericValidation[EitherValidation]

  val ue1 = eitherValidation.validateUser("Bob", 29, "bob@bob.com")

  println(ue1)

  val ue2 = eitherValidation.validateUser("Bob", -2, "bob@bob.com")

  println(ue2)

  val ue3 = eitherValidation.validateUser("Bob", -2, "bobrules!")

  println(ue3)
}