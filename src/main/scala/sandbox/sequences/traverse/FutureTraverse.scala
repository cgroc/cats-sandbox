package sandbox.sequences.traverse

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import cats.Traverse
import cats.instances.list._
import cats.instances.future._

object FutureTraverse {

  val hostNames: List[String] = List("huey", "duey", "louie")

  def uptime(hostname: String): Future[Int] =
    Future.successful(hostname.length)

  def totalUptime(hostnames: List[String]): Future[Int] =
    Traverse[List].traverse(hostnames)(uptime).map(_.sum)

  def main(args: Array[String]): Unit = {
    val time: Int = Await.result(totalUptime(hostNames), 1.second)

    println(time)
  }
}
