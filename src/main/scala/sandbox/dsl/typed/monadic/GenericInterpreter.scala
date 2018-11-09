package sandbox.dsl.typed.monadic

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

class GenericInterpreter[F[_]](implicit monad: Monad[F]) {

  def eval[A](expr: Expr[A]): F[A] =
    expr match {
      case Pure(a)     => a.pure
      case Add(n1, n2) =>
        (n1 + n2).pure
      case Mul(n1, n2) =>
        (n1 * n2).pure
      case And(b1, b2) =>
        (b1 && b2).pure
      case Gte(n1, n2) =>
        (n1 >= n2).pure
      case FlatMapExpr(expA, f) =>
        for {
          a <- eval(expA)
          b <- eval(f(a))
        } yield b

    }
}
