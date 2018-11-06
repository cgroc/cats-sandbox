package sandbox.minimapreduce

import java.util.concurrent.Executors

import cats.{Monoid, Traverse}
import cats.instances.list._
import cats.instances.future._
import cats.instances.int._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

object MiniMapReduce {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())

  val numCores: Int = Runtime.getRuntime.availableProcessors()
  def itemsPerNode[A]: List[A] => Int = values =>  (1.0 * values.size / numCores).ceil.toInt

  // inputData (has a Traverse)
  // mappingOp
  // reducingOp (has a Monoid)

  def mapReduce[A, B](data: List[A])(func: A => B)(implicit M: Monoid[B]): B = {
    println(s"Data size is: ${data.length}, running on ${Thread.currentThread().getName}")
    data.map(func).foldLeft(M.empty)(M.combine)
  }

  // note: map followed by sequence = traverse
  def parallelMapReduce[A, B](data: List[A])(func: A => B)(implicit M: Monoid[B]): Future[B] = {
    val partitionedJobs: List[List[A]] = data.grouped(itemsPerNode(data)).toList
    val listFutures: List[Future[B]] = partitionedJobs.map(l => Future(mapReduce(l)(func)))
    val futureList: Future[List[B]] = Traverse[List].sequence(listFutures)
    futureList.map(_.foldLeft(M.empty)(M.combine))
  }

  def main(args: Array[String]): Unit = {
    val futureResult: Future[Int] = parallelMapReduce((0 to 999).toList)(_ + 1)

    val result: Int = Await.result(futureResult, 10.seconds)

    println(numCores)

    println(result)

    println((1 to 1000).toList.sum)
  }

}
