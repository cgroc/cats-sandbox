package sandbox.dsl.untyped.basic2

sealed trait Expr

case class Lit(v: Val) extends Expr

case class Add(a: Expr, b: Expr) extends Expr
case class Mul(a: Expr, b: Expr) extends Expr

case class And(a: Expr, b: Expr) extends Expr
case class Gte(a: Expr, b: Expr) extends Expr

sealed trait Val
case class NumVal(value: Int) extends Val
case class BoolVal(value: Boolean) extends Val
