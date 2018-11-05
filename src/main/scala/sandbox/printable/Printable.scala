package sandbox.printable

trait Printable[A] {

  def asString(a: A): String

  def print(a: A): Unit =
    println(asString(a))
}

object Printable {

  implicit val printableInt: Printable[Int] = new Printable[Int] {
    def asString(a: Int): String =
      s"HOLY CRAP IT'S $a!!!"
  }
}

object PrintableFunctions {

  def stringify[A](value: A)(implicit printable: Printable[A]): String =
    printable.asString(value)
}