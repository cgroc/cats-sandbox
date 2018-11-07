package sandbox.dsl.typed.monadic

import cats.{Monad, StackSafeMonad}

sealed trait Expr[A]

case class Pure[A](a: A) extends Expr[A]

case class Add(a: Expr[Int], b: Expr[Int]) extends Expr[Int]
case class Mul(a: Expr[Int], b: Expr[Int]) extends Expr[Int]

case class And(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean]
case class Gte(a: Expr[Int], b: Expr[Int]) extends Expr[Boolean]

case class FlatMapExpr[A, B](fa: Expr[A], f: A => Expr[B]) extends Expr[B]

object Expr {

  implicit val exprMonad: Monad[Expr] = new Monad[Expr] with StackSafeMonad[Expr] {
    override def pure[A](a: A): Expr[A] = Pure(a)

    override def flatMap[A, B](fa: Expr[A])(f: A => Expr[B]): Expr[B] = FlatMapExpr(fa, f)

  }

}
