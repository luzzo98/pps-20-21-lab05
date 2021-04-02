package u05lab.code

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class SomeTest {

  @Test
  def testZipRight() {
    val l = List("a", "b", "c")

    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a",0), ("b",1), ("c",2)), l.zipRight)
  }

  @Test
  def testPartitions() {
    val l = List(2,4,5,8,9)
    val l2 = List("a", "a", "a", "a", "b", "c", "d", "a")

    assertEquals((List.nil, List.nil), List.nil[Int].partition(_%2==0))
    assertEquals((List(2,4,8),List(5,9)), l.partition(_%2==0))
    assertEquals((List("a", "a", "a", "a", "a"),List("b", "c", "d")), l2.partition(_=="a"))
  }

  @Test
  def testSpan() {
    val l = List(2,4,5,8,9)
    val l2 = List("a", "a", "a", "a", "b", "c", "d", "a")

    assertEquals((List.nil, List.nil), List.nil[Int].span(_%2==0))
    assertEquals((List(2,4),List(5,8,9)), l.span(_%2==0))
    assertEquals((List("a", "a", "a", "a"),List("b", "c", "d", "a")), l2.span(_=="a"))
  }

  @Test
  def testReduceLeft(): Unit = {
    val l = List(1,2,3,4)
    val l2 = List("a", "b", "c", "d")

    assertThrows(classOf[UnsupportedOperationException], () => List.nil[Int].reduceLeft(_+_))
    assertEquals(1, List(1).reduceLeft(_+_))
    assertEquals(10, l.reduceLeft(_+_))
    assertEquals(-8, l.reduceLeft(_-_))
    assertEquals("abXcXdX", l2.reduceLeft(_+_+"X"))
  }

  @Test
  def testReduceRight(): Unit = {
    val l = List(1,2,3,4)
    val l2 = List("a", "b", "c", "d")

    assertThrows(classOf[UnsupportedOperationException], () => List.nil[Int].reduceRight(_+_))
    assertEquals(1, List(1).reduceRight(_+_))
    assertEquals(-2, l.reduceRight(_-_))
    assertEquals("abcdXXX", l2.reduceRight(_+_+"X"))
  }

  @Test
  def testTakeRight(): Unit = {
    val l = List(1,2,3,4,5,6,7,8,9)
    val l2 = List("a", "b", "c", "d", "e", "f", "g")

    assertEquals(List.nil, List.nil[Int].takeRight(5))
    assertEquals(List(5,6,7,8,9), l.takeRight(5))
    assertEquals(List("f", "g"), l2.takeRight(2))
    assertEquals(List.nil, l.takeRight(0))
    assertEquals(List.nil, l.takeRight(-3))
  }

  @Test
  def collect(): Unit = {
    val l = List(10,20,30,40)
    val l2 = List("a","b","c","b","a")

    assertEquals(List.nil, List.nil[Int].collect{ case x if x<15 || x>35 => x-1 })
    assertEquals(List(9,39), l.collect { case x if x<15 || x>35 => x-1 })
    assertEquals(List("aX","cX","aX"), l2.collect { case x if x=="a" || x=="c" => x+"X"})
  }
}