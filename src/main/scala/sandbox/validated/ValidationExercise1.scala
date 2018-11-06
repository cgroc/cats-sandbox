package sandbox.validated

import cats.data.Validated
import cats.instances.string._
import cats.syntax.apply._

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
