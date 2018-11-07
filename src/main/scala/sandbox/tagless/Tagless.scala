package sandbox.tagless

import cats.{Applicative, Monad}
import cats.syntax.all._

trait Expr[F[_]] {
  def add(x: Int, y: Int): F[Int]
  def mul(x: Int, y: Int): F[Int]
  def lt(x: Int, y: Int): F[Boolean]
  def and(a: Boolean, b: Boolean): F[Boolean]
}

class GenericInterpreter[F[_]](implicit applicative: Applicative[F]) extends Expr[F] {
  def add(x: Int, y: Int): F[Int] =
    (x + y).pure

  def mul(x: Int, y: Int): F[Int] =
    (x * y).pure

  def lt(x: Int, y: Int): F[Boolean] =
    (x < y).pure

  def and(a: Boolean, b: Boolean): F[Boolean] =
    (a && b).pure
}

class Program[F[_]](expr: Expr[F])(implicit monad: Monad[F]) {

  def run: F[Int] =
    for {
      a <- 7.pure
      b <- 11.pure
      c <- expr.mul(a, b)
    } yield c
}

object App {

  def main(args: Array[String]): Unit = {
    import cats.instances.option._

    val interpreter = new GenericInterpreter[Option]

    val program = new Program[Option](interpreter)

    println(program.run)
  }
}