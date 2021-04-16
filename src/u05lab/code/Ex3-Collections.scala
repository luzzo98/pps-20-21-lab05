package u05lab.code

import java.util.concurrent.TimeUnit
import scala.collection.LinearSeq
import scala.collection.immutable.{HashSet, ListSet}
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime()-startTime, TimeUnit.NANOSECONDS)
    if(msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)
}


object CollectionsTest extends App {
  import PerformanceUtils._
  val lst = (1 to 1000000).toList
  val vec = (1 to 1000000).toVector
  assert( measure("lst"){ lst.last } > measure("vec"){ vec.last } )
  println

  val iSeq = IndexedSeq(1 to 1000000)
  val lSeq = LinearSeq(1 to 1000000)
  assert( measure("iSeq"){ iSeq.foreach(_+"!") } > measure("lSeq"){ lSeq.foreach(_+"!") } )
  println

  val lSet = ListSet(1 to 1000000)
  val hSet = HashSet(1 to 1000000)
  assert( measure("lSet"){ lSet.foreach(_+"!") } > measure("hSet"){ hSet.foreach(_+"!") } )
  println
}