package sandbox.dsl.basic1.untyped

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

/*
 * This was my first crack, but of course you don't need a Monad! You
 * can get away with Applicative!
 */

class GenericMonadInterpreter[F[_]: Monad]() {

  def eval(expr: Expr): F[Int] =
    expr match {
      case Num(v)      =>
        v.pure[F]
      case Add(e1, e2) =>
        for {
          r1 <- eval(e1)
          r2 <- eval(e2)
        } yield r1 + r2
      case Mul(e1, e2) =>
        for {
          r1 <- eval(e1)
          r2 <- eval(e2)
        } yield r1 * r2
    }

}
