package sandbox.dsl.untyped.basic2

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._

// had to retreat from Applicative bcs there are 2 stages now!
class GenericInterpreter[F[_]](implicit monadError: MonadError[F, String]) {

  def eval(expr: Expr): F[Val] =
    expr match {
      case Lit(v)      =>
        v.pure[F]
      case Add(e1, e2) =>
        for {
          v1 <- eval(e1)
          v2 <- eval(e2)
          r  <- combineAdd(v1, v2)
        } yield r
      case Mul(e1, e2) =>
        for {
          v1 <- eval(e1)
          v2 <- eval(e2)
          r  <- combineMultiply(v1, v2)
        } yield r
      case And(e1, e2) =>
        for {
          v1 <- eval(e1)
          v2 <- eval(e2)
          r  <- combineAnd(v1, v2)
        } yield r
      case Gte(e1, e2) =>
        for {
          v1 <- eval(e1)
          v2 <- eval(e2)
          r  <- combineGte(v1, v2)
        } yield r
    }

  def combineAdd(v1: Val, v2: Val): F[Val] =
    (v1, v2) match {
      case (NumVal(n1), NumVal(n2)) => (NumVal(n1 + n2): Val).pure[F] // bleugh
      case _ => "Can't add that!".raiseError
    }

  def combineMultiply(v1: Val, v2: Val): F[Val] =
    (v1, v2) match {
      case (NumVal(n1), NumVal(n2)) => (NumVal(n1 * n2): Val).pure[F] // bleugh
      case _ => "Can't multiply that!".raiseError
    }

  def combineAnd(v1: Val, v2: Val): F[Val] =
    (v1, v2) match {
      case (BoolVal(b1), BoolVal(b2)) => (BoolVal(b1 && b2): Val).pure[F] // bleugh
      case _ => "Can't and that!".raiseError
    }

  def combineGte(v1: Val, v2: Val): F[Val] =
    (v1, v2) match {
      case (NumVal(n1), NumVal(n2)) => (BoolVal(n1 >= n2): Val).pure[F] // bleugh
      case _ => "Can't gte that!".raiseError
    }
}
