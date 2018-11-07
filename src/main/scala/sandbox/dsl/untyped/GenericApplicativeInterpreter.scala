package sandbox.dsl.untyped

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.apply._

/*
 * Woot! The monad isn't necessary!
 */

class GenericApplicativeInterpreter[F[_]: Applicative]() {

  def eval(expr: Expr): F[Int] =
    expr match {
      case Num(v)      => v.pure[F]
      case Add(e1, e2) =>
        (
          eval(e1),
          eval(e2)
        ).mapN(_ + _)
      case Mul(e1, e2) =>
        (
          eval(e1),
          eval(e2)
        ).mapN(_ * _)
    }

}
