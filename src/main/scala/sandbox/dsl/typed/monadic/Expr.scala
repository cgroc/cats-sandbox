package sandbox.dsl.typed.monadic

import cats.{Monad, StackSafeMonad}

sealed trait Expr[A]

case class Pure[A](a: A) extends Expr[A]

case class Add(a: Int, b: Int) extends Expr[Int]
case class Mul(a: Int, b: Int) extends Expr[Int]

case class And(a: Boolean, b: Boolean) extends Expr[Boolean]
case class Gte(a: Int, b: Int) extends Expr[Boolean]

case class FlatMapExpr[A, B](fa: Expr[A], f: A => Expr[B]) extends Expr[B]

object Expr {

  def pure[A](a: A): Expr[A] = Pure(a)
  def add(a: Int, b: Int): Expr[Int] = Add(a, b)
  def mul(a: Int, b: Int): Expr[Int] = Mul(a, b)
  def and(a: Boolean, b: Boolean): Expr[Boolean] = And(a, b)
  def gte(a: Int, b: Int): Expr[Boolean] = Gte(a, b)

  implicit val exprMonad: Monad[Expr] = new StackSafeMonad[Expr] {
    override def pure[A](a: A): Expr[A] = Pure(a)

    override def flatMap[A, B](fa: Expr[A])(f: A => Expr[B]): Expr[B] = FlatMapExpr(fa, f)

  }

}
