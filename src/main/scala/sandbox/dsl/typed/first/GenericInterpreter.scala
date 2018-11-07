package sandbox.dsl.typed.first

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

class GenericInterpreter[F[_]](implicit monad: Monad[F]) {

  def eval[A](expr: Expr[A]): F[A] =
    expr match {
      case Val(a) => a.pure
      case Add(n1, n2) =>
        for {
          a <- eval(n1)
          b <- eval(n2)
        } yield a + b
      case Mul(n1, n2) =>
        for {
          a <- eval(n1)
          b <- eval(n2)
        } yield a * b
      case And(b1, b2) =>
        for {
          a <- eval(b1)
          b <- eval(b2)
        } yield a && b
      case Gte(n1, n2) =>
        for {
          a <- eval(n1)
          b <- eval(n2)
        } yield a >= b
    }
}
