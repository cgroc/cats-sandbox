package sandbox.dsl.untyped.basic2

object App {

  import cats.instances.either._

  type CalcResult[Val] = Either[String, Val]

  def main(args: Array[String]): Unit = {
    val expression = Add(Mul(Lit(NumVal(2)), Lit(NumVal(3))), Lit(NumVal(4)))

    val genericInterpreter = new GenericInterpreter[CalcResult]

    val otherOptionResult = genericInterpreter.eval(expression)

    println(otherOptionResult)
  }
}
