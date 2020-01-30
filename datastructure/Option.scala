

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def map2[A, B, C](op1: Option[A], op2: Option[B])(f: (A, B) => C): Option[C] = (op1, op2) match {
    case (None, _)          => None
    case (_, None)          => None
    case (Some(x), Some(y)) => Some(f(x, y))
  }
  //  def map3[A, B, C](op1: Option[A], op2: List[Option[B]])(f: (A, List[B]) => C): Option[C] = (op1, op2) match {
  //    case (None, _)          => None
  //    case (_, Nil)          => None
  //    case (Some(x), l) => Some(f(x, l))
  //  }
  def seq[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil          => Some(Nil)
    case List(None)   => Some(Nil)
    case Some(h) :: t => seq(t).map(x => h :: x)
  }
  def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = l match {
    case Nil    => Some(Nil)
    case h :: t => traverse(t)(f).flatMap(x => Try(f(h)).flatMap(b => b.map(c => c :: x)))
  }
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None}
  }
}
