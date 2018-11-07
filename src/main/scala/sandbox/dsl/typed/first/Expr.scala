package sandbox.dsl.typed.first

sealed trait Expr[A]

case class Val[A](a: A) extends Expr[A]

case class Add(a: Expr[Int], b: Expr[Int]) extends Expr[Int]
case class Mul(a: Expr[Int], b: Expr[Int]) extends Expr[Int]

case class And(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean]
case class Gte(a: Expr[Int], b: Expr[Int]) extends Expr[Boolean]

//
//sealed trait Val
//case class NumVal(value: Int) extends Val
//case class BoolVal(value: Boolean) extends Val
