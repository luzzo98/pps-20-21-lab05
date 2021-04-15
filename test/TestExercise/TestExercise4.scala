package TestExercise

import u05lab.code._
import u05lab.code.Exercise4._
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class TestExercise4 {

  @Test
  def testSequence(): Unit = {
    assertEquals(Some(List(1,2,3)), sequence(List[Option[Int]](Some(1), Some(2), Some(3))))
    assertEquals(None, sequence(List[Option[Int]](Some(1), None, Some(3))))
  }
}
