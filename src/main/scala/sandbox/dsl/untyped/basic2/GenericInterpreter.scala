package sandbox.dsl.untyped.basic2

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._

// had to retreat from Applicative bcs there are 2 stages now!

// other things to put in
// scope?
// conditionals
class GenericInterpreter[F[_]](implicit monadError: MonadError[F, String]) {

  def eval(expr: Expr): F[Val] =
    expr match {
      case Lit(v)      =>
        v.pure[F]
      case Add(e1, e2) =>
        for {
          v1 <- eval(e1)
          n1 <- valToNumVal(v1)
          v2 <- eval(e2)
          n2 <- valToNumVal(v2)
        } yield NumVal(n1 + n2)
      case Mul(e1, e2) =>
        for {
          v1 <- eval(e1)
          n1 <- valToNumVal(v1)
          v2 <- eval(e2)
          n2 <- valToNumVal(v2)
        } yield NumVal(n1 * n2)
      case And(e1, e2) =>
        for {
          v1 <- eval(e1)
          b1 <- valToBoolVal(v1)
          v2 <- eval(e2)
          b2 <- valToBoolVal(v2)
        } yield BoolVal(b1 && b2)
      case Gte(e1, e2) =>
        for {
          v1 <- eval(e1)
          n1 <- valToNumVal(v1)
          v2 <- eval(e2)
          n2 <- valToNumVal(v2)
        } yield BoolVal(n1 >= n2)
    }


  def valToNumVal(v: Val): F[Int] =
    v match {
      case NumVal(n) => n.pure[F]
      case _         => "Can't convert to Int".raiseError
    }

  def valToBoolVal(v: Val): F[Boolean] =
    v match {
      case BoolVal(b) => b.pure[F]
      case _         => "Can't convert to Boolean".raiseError
    }
}
