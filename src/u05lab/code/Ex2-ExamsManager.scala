package u05lab.code

import u05lab.code.Kind.{Failed, Retired, Succeeded}
import scala.collection._

trait Kind
object Kind {
  case class Retired() extends Kind {
    override def toString: String = "RETIRED"
  }
  case class Failed() extends Kind {
    override def toString: String = "FAILED"
  }
  case class Succeeded() extends Kind {
    override def toString: String = "SUCCEEDED"
  }
}

trait ExamResult {
  def getKind: Kind
  def getEvaluation: Option[Int]
  def cumLaude: Boolean

  override def toString: String = {
    var s = ""
    if (cumLaude) s="(30L)"
    else if (getEvaluation.isDefined) s= "(" + getEvaluation.get.toString + ")"
    getKind.toString + s
  }
}

trait ExamResultFactory {

  def failed: ExamResult
  def retired: ExamResult
  def succeededCumLaude: ExamResult
  def succeeded(evaluation: Int): ExamResult
}


trait ExamManager {

  def createNewCall(call: String): Unit

  def addStudentResult(call: String, student: String, result: ExamResult): Unit

  def getAllStudentsFromCall(call: String): Set[String]

  def getEvaluationsMapFromCall(call: String): Map[String,Int]

  def getResultsMapFromStudent(student: String): Map[String,String]

  def getBestResultFromStudent(student: String): Option[Int]
}


object ExamResultFactory {
  def apply():ExamResultFactory = ExamResultFactoryImpl()
}
object ExamManager {
  def apply():ExamManager = ExamsManagerImpl()
}


case class ExamResultFactoryImpl() extends ExamResultFactory {
  abstract class AbstractExam(kind: Kind) extends ExamResult {
    override def getKind: Kind = kind
    override def getEvaluation: Option[Int] = Option.empty
    override def cumLaude: Boolean = false
  }
  abstract class AbstractSucceededExam(evaluation: Int) extends ExamResult {
    if (evaluation>30 || evaluation<18) throw new IllegalArgumentException
    override def getKind: Kind = Succeeded()
    override def getEvaluation: Option[Int] = Option(evaluation)
    override def cumLaude: Boolean = false
  }

  override def failed: ExamResult = new AbstractExam(Failed()){}

  override def retired: ExamResult = new AbstractExam(Retired()){}

  override def succeededCumLaude: ExamResult = new AbstractSucceededExam(30) {override def cumLaude: Boolean = true}

  override def succeeded(evaluation: Int): ExamResult = new AbstractSucceededExam(evaluation){}
}


case class ExamsManagerImpl() extends ExamManager {
  private val map: mutable.Map[String, Map[String, ExamResult]] = mutable.Map()

  override def createNewCall(call: String): Unit =
    if (!map.contains(call)) map += (call -> Map.empty) else throw new IllegalArgumentException()

  override def addStudentResult(call: String, student: String, result: ExamResult): Unit =
    if (!map(call).contains(student)) map(call) += (student -> result) else throw new IllegalArgumentException()

  override def getAllStudentsFromCall(call: String): Set[String] = map(call).keySet

  override def getEvaluationsMapFromCall(call: String): Map[String, Int] = {
    var m: Map[String, Int] = Map()
    map(call).foreach(k => if (k._2.getEvaluation.isDefined) m+=(k._1 -> k._2.getEvaluation.get))
    m
  }

  override def getResultsMapFromStudent(student: String): Map[String, String] = {
    var m: Map[String, String] = Map()
    map.foreach(k => k._2.foreach(x => if (x._1 == student) m+=(k._1 -> x._2.toString)))
    m
  }

  override def getBestResultFromStudent(student: String): Option[Int] = {
    var res = 0
    map.foreach(k => k._2.foreach(x => if (x._1 == student)
      if (x._2.getEvaluation.isDefined)
        if (x._2.getEvaluation.get > res) res = x._2.getEvaluation.get
    ))
    if (res==0) Option.empty else Option(res)
  }
}