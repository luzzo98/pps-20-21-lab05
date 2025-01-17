package u05lab.code

object Exercise4 {

  // third version with map2
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Option(List[A]())) ((v,l) => map2(v)(l)((a,b) => a::b))

  // map2 of the lab02
  def map2[A,B,C](opt1: Option[A])(opt2: Option[B])(f: (A, B) => C): Option[C] = (opt1, opt2) match {
    case (Some(a), Some(b)) => Some(f(a,b))
    case _ => None
  }


  // second version
//  def sequence[A](a: List[Option[A]]): Option[List[A]] =
//    a.foldRight(Option(List[A]())) ((v,l) => if (v.isDefined && l.isDefined) Some(v.get :: l.get) else None)


  // first version
//  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
//    var isValid = true
//    var l = List[A]()
//    a.foreach(v => if (v.isDefined) l = v.get :: l else isValid = false)
//    if (l == Nil[A]() || !isValid ) None else Some(l.reverse())
//  }
}
