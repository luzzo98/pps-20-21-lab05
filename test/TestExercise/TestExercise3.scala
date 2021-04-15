package TestExercise

import collection._
import org.junit.jupiter.api.Test

class TestExercise3 {

  @Test
  def testLinearSequences(): Unit = {
    //  val mutableList = collection.mutable.MutableList[Int]() // it doesn't have the -= operator
    val mutableList = mutable.ListBuffer[Int]() // note "val"
    var immutableList = immutable.List[Int]() // note "var"
    (1 to 10).foreach(v => {
      mutableList += v
      immutableList = immutableList :+ v
    })
    println("--------------Linear sequences---------------")
    println("m : " + mutableList.toList)
    println("im: " + immutableList)
    println("m : " + mutableList(3) + ", " + mutableList.size )
    println("im: " + immutableList(3) + ", " + immutableList.size)

    mutableList(5) = -5
    immutableList = immutableList.updated(5,-5)
    println("m : " + mutableList.toList)
    println("im: " + immutableList)

    mutableList -= 5
    immutableList = immutableList.filter(_ != 5) //you can't delete elements but you can filter
    println("m : " + mutableList.toList)
    println("im: " + immutableList)
    println
  }

  @Test
  def testIndexedSequences(): Unit = {
    val mutableArray = mutable.ArrayBuffer[Int]() // note "val"
    var immutableArray = immutable.Vector[Int]() // note "var"
    (1 to 10).foreach(v => {
      mutableArray += v
      immutableArray = immutableArray :+ v
    })
    println("--------------Indexed sequences--------------")
    println("m : " + mutableArray.toVector)
    println("im: " + immutableArray)
    println("m : " + mutableArray(3) + ", " + mutableArray.size )
    println("im: " + immutableArray(3) + ", " + immutableArray.size)

    mutableArray(5) = -5
    immutableArray = immutableArray.updated(5,-5)
    println("m : " + mutableArray.toVector)
    println("im: " + immutableArray)

    mutableArray -= 5
    immutableArray = immutableArray.filter(_ != 5) //you can't delete elements but you can filter
    println("m : " + mutableArray.toVector)
    println("im: " + immutableArray)
    println
  }

  @Test
  def testSets(): Unit = {
    val mutableSet = mutable.HashSet[Int]() // note "val"
    var immutableSet = immutable.HashSet[Int]() // note "var"
    (1 to 10).foreach(v => {
      mutableSet += v
      immutableSet = immutableSet + v
    })
    println("--------------------Sets---------------------")
    println("m : " + mutableSet) // the set isn't ordered so this stamp it's different from the next
    println("im: " + immutableSet)
    println("m : " + mutableSet.head + ", " + mutableSet.size ) // in set you can't use index
    println("im: " + immutableSet.head + ", " + immutableSet.size) // this element can be different from the other

    mutableSet -= 6; mutableSet += -5 // to update an element you can delete it and add a new one...
    immutableSet = immutableSet.map( v => if (v == 6) -5 else v) // ...or you can use a map and take the returned Set as in the immutable version (but you need "var")
    println("m : " + mutableSet)
    println("im: " + immutableSet)

    mutableSet -= 5
    immutableSet = immutableSet.filter(_ != 5) //you can't delete elements but you can filter
    println("m : " + mutableSet)
    println("im: " + immutableSet)
    println
  }

  @Test
  def testMaps(): Unit = {
    val mutableMap = mutable.HashMap[Int,String]() // note "val"
    var immutableMap = immutable.HashMap[Int,String]() // note "var"
    (1 to 5).foreach(v => {
      mutableMap += v -> v.toString
      immutableMap = immutableMap + (v -> v.toString)
    })
    println("--------------------Maps---------------------")
    println("m : " + mutableMap) // the map isn't ordered so this stamp it's different from the next
    println("im: " + immutableMap)
    println("m : " + mutableMap(3) + ", " + mutableMap.size )
    println("im: " + immutableMap(3) + ", " + immutableMap.size)

    mutableMap(5) = "-5"
    immutableMap = immutableMap.updated(5,"-5")
//    immutableMap = immutableMap.map(v => if (v._1==5) 5 -> "-5" else v)
    println("m : " + mutableMap)
    println("im: " + immutableMap)

    mutableMap -= 5 // or mutableMap -= (5,-5)
    immutableMap = immutableMap.filter(_._1 != 5) //you can't delete elements but you can filter
    println("m : " + mutableMap)
    println("im: " + immutableMap)
    println
  }
}
