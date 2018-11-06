package sandbox.functors

import cats.Functor
import cats.syntax.functor._

sealed trait Baum[A]
case class Leaf[A](value: A) extends Baum[A]
case class Node[A](left: Baum[A], right: Baum[A]) extends Baum[A]

object Baum {

  implicit val baumFunctor: Functor[Baum] = new Functor[Baum] {
    def map[A, B](fa: Baum[A])(f: A => B): Baum[B] =
     fa match {
       case Leaf(a)     => Leaf(f(a))
       case Node(l, r)  => Node(map(l)(f), map(r)(f))
     }
  }
}

// my implementation of Functor, replace this with cats!
//trait MyFunctor[F[_]] {
//
//  def map[A, B](fa: F[A])(f: A => B): F[B]
//
//}

//object MyFunctor {
//
//  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
//    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
//  }
//
//  implicit val listFunctor: Functor[List] = new Functor[List] {
//    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
//  }
//}

object Stuff {

  def functorAddN[F[_]](input: F[Int], n: Int)(implicit functorEv: Functor[F]): F[Int] =
    input.map(_ + n)
//    functorEv.map(input)(_ + n)
}

object App {

  def main(args: Array[String]): Unit = {
    import cats.instances.option._

    println(implicitly[Functor[Option]].map(Some(1))(_ + 1))
    println(implicitly[Functor[Baum]].map(Node(Leaf(1), Leaf(2)))(_ + 8))

    println(Functor[Baum].map(Node(Leaf(1), Leaf(2)))(_ * 1000))

    println(
      (Node(Leaf(1), Leaf(2)): Baum[Int]).map(_ + 1)
    )

    val theLarch: Baum[Int] = Node(Leaf(1), Leaf(2))

    println(
      Stuff.functorAddN(
        theLarch, 8
      )
    )

  }
}