package sandbox.dsl.basic1.untyped

sealed trait Expr

case class Num(value: Int) extends Expr
case class Add(a: Expr, b: Expr) extends Expr
case class Mul(a: Expr, b: Expr) extends Expr
