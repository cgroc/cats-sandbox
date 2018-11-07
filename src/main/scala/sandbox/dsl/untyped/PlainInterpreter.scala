package sandbox.dsl.untyped

object PlainInterpreter {

  def eval(expr: Expr): Int =
    expr match {
      case Num(v)      => v
      case Add(e1, e2) => eval(e1) + eval(e2)
      case Mul(e1, e2) => eval(e1) * eval(e2)
    }
}
