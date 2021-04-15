package u05lab.code

object Exercise4 {

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Option(List[A]())) ((v,l) => if (v.isDefined && l.isDefined) Some(v.get :: l.get) else None)


  // first version
//  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
//    var isValid = true
//    var l = List[A]()
//    a.foreach(v => if (v.isDefined) l = v.get :: l else isValid = false)
//    if (l == Nil[A]() || !isValid ) None else Some(l.reverse())
//  }
}
