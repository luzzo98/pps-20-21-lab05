package TestExercise

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u05lab.code.List

class TestExercise1 {

  @Test
  def testZipRight() {
    val l = List("a", "b", "c")
    val l2 = List(true, false, true)

    assertEquals(List.nil, List.nil.zipRight) // test with Nil
    assertEquals(List(("a",0), ("b",1), ("c",2)), l.zipRight)
    assertEquals(List((true,0), (false,1), (true,2)), l2.zipRight)
  }

  @Test
  def testPartitions() {
    val l = List(2,4,5,8,9)
    val l2 = List("a", "a", "b", "c", "d", "a")

    assertEquals((List.nil, List.nil), List.nil[Int].partition(_%2==0)) // test with Nil
    assertEquals((List(2,4,8),List(5,9)), l.partition(_%2==0))
    assertEquals((List("a", "a", "a"),List("b", "c", "d")), l2.partition(_=="a"))
  }

  @Test
  def testSpan() {
    val l = List(2,4,5,8,9)
    val l2 = List("a", "a", "b", "c", "d", "a")

    assertEquals((List.nil, List.nil), List.nil[Int].span(_%2==0)) // test with Nil
    assertEquals((List(2,4),List(5,8,9)), l.span(_%2==0)) // 8 is in the second list
    assertEquals((List("a", "a"),List("b", "c", "d", "a")), l2.span(_=="a")) // the last "a" is in the second list
  }

  @Test
  def testReduceLeft(): Unit = {
    val l = List(1,2,3,4)
    val l2 = List("a", "b", "c", "d")

    assertThrows(classOf[UnsupportedOperationException], () => List.nil[Int].reduceLeft(_+_)) // test with Nil
    assertEquals(1, List(1).reduceLeft(_+_)) // single element list
    assertEquals(10, l.reduceLeft(_+_)) // ((1+2) +3) +4
    assertEquals(-8, l.reduceLeft(_-_)) // ((1-2) -3) -4 = (-1 - 3) -4 = -4 - 4 = -8
    assertEquals("abXcXdX", l2.reduceLeft(_+_+"X"))
  }

  @Test
  def testReduceRight(): Unit = {
    val l = List(1,2,3,4)
    val l2 = List("a", "b", "c", "d")

    assertThrows(classOf[UnsupportedOperationException], () => List.nil[Int].reduceRight(_+_)) // test with Nil
    assertEquals(1, List(1).reduceRight(_+_)) // single element list
    assertEquals(10, l.reduceRight(_+_)) // 1+ (2+ (3+4))
    assertEquals(-2, l.reduceRight(_-_)) // 1- (2- (3-4)) = 1- (2-(-1)) = 1-3 = -2
    assertEquals("abcdXXX", l2.reduceRight(_+_+"X"))
  }

  @Test
  def testTakeRight(): Unit = {
    val l = List(1,2,3,4,5,6,7,8,9)
    val l2 = List("a", "b", "c", "d", "e", "f", "g")

    assertEquals(List.nil, List.nil[Int].takeRight(5)) // test with Nil
    assertEquals(List.nil, l.takeRight(0)) // take with 0
    assertEquals(List.nil, l.takeRight(-3)) // take with a negative number (same behaviour as with 0)
    assertEquals(List(5,6,7,8,9), l.takeRight(5))
    assertEquals(List("f", "g"), l2.takeRight(2))
  }

  @Test
  def collect(): Unit = {
    val l = List(10,20,30,40)
    val l2 = List("a","b","c","b","a")

    assertEquals(List.nil, List.nil[Int].collect { case x if x<15 || x>35 => x-1 } ) // test with Nil
    assertEquals(List(9,39), l.collect { case x if x<15 || x>35 => x-1 } )
    assertEquals(List("aX","cX","aX"), l2.collect { case x if x=="a" || x=="c" => x+"X"} )
  }
}