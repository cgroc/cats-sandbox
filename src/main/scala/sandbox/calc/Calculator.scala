package sandbox.calc

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._

trait Calculator[F[_]] {

  def add(x: Int, y: Int): F[Int]

  def mul(x: Int, y: Int): F[Int]

  def sumSquare(x: Int, y: Int): F[Int]
}

object Calculator {

  type Result[A] = Either[String, A]

}

class GenericCalculator[F[_]](implicit monadError: MonadError[F, String]) extends Calculator[F] {

  def add(x: Int, y: Int): F[Int] =
    (x + y).pure

  def mul(x: Int, y: Int): F[Int] =
    (x * y).pure

  def div(x: Int, y: Int): F[Int] =
    if(y == 0)
      "Divide by zero!".raiseError
    else
      (x / y).pure

  def sumSquare(x: Int, y: Int): F[Int] =
    for {
      a2  <- mul(x, x)
      b2  <- mul(y, y)
      res <- add(a2, b2)
    } yield res
}

object App {

  def main(args: Array[String]): Unit = {

    import Calculator._
    import cats.instances.either.catsStdInstancesForEither // import it here to avoid scope clashes!

    val calc = new GenericCalculator[Result]

    val res1 = calc.div(3, 2)

    println(res1)

    val res2 = calc.div(3, 0)

    println(res2)
  }
}
