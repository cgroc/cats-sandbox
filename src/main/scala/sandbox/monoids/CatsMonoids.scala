package sandbox.monoids

import cats.Monoid

object CatsMonoids {

  def addAll(list: List[Int]): Int =
    list.foldLeft(0)(_ + _)

  def combineAll[A: Monoid](list: List[A]): A = {
    val m = implicitly[Monoid[A]]
    list.foldLeft(m.empty)(m.combine)
  }

  def alsoCombineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldLeft(monoid.empty)(monoid.combine)

  def main(args: Array[String]): Unit = {
    import cats.instances.int._
    println(combineAll(List(1, 2, 3, 4)))
  }
}
