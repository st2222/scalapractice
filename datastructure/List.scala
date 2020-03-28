
//Listと被るため名前を変更
sealed trait Lst[+A]
case object Nill extends Lst[Nothing]
case class Conss[+A](head: A, tail: Lst[A]) extends Lst[A]

object Lst {
  
  def apply[A](as: A*): Lst[A] =  {
    if(as.isEmpty) Nill
    else Conss(as.head, apply(as.tail: _*))
  }
  
  def tail[A](lst: Lst[A]): Lst[A] = lst match {
    case Nill => Nill
    case Conss(_, t) => t
  }
  
  def setHead[A](lst: Lst[A], head: A) = lst match {
    case Nill => Nill
    case Conss(h, t) => Conss(head, t)
    
  }
  
  def drop[A](lst: Lst[A], n: Int): Lst[A] = 
    if (n == 0) lst else {
      lst match {
        case Nill => Nill
        case Conss(h, t) => drop(t, n-1)
    }
  }
  
  def dropWhile[A](l: Lst[A])(f: A => Boolean): Lst[A] = {
    l match {
//      case Nill => Nill
//      case Conss(h, t) => if (f(h)) dropWhile(t, f) else t
      case Conss(h, t) if(f(h)) => dropWhile(t)(f)
      case _ => l
    }
  }
  
  def foldRight[A, B](lst: Lst[A], z: B)(f: (A, B) => B): B = lst match {
    case Nill => z
    case Conss(h, t) => f(h, foldRight(t, z)(f))
  }
  
  def length[A](as: Lst[A]): Int = {
    foldRight(as, 0)((_, b) => 1 + b) 
  }
  
  @annotation.tailrec
  def foldLeft[A, B](as: Lst[A], z: B)(f: (B, A) => B): B = as match {
    case Nill => z
    case Conss(h, t) => foldLeft(t, f(z, h))(f)
  }
  
  def reverse[A](as: Lst[A]): Lst[A] = foldLeft(as, Lst[A]())((a, b) => Conss(b, a))
  
  def append[A](as: Lst[A], a: Lst[A]): Lst[A] = foldRight(as, a)(Conss(_, _))//foldLeft(as, Lst(a))((b, c) => Conss(b, c))//foldRight(as, Lst(a))((b, c) => Conss(b, c))
  
  def map[A, B](as: Lst[A])(f: A => B): Lst[B] = foldLeft(as, Nill: Lst[B])((a, b) => Lst.append(a, Lst(f(b))))
  
  def map2[A, B](as: List[A])(f: A=> B): List[B] = as.foldRight(Nil: List[B])((a, b) => f(a) :: b)
  
  def map3[A, B](as: Lst[A])(f: A => B): Lst[B] = foldRight(as, Nill: Lst[B])((a, b) => Conss(f(a), b))
  
  def filter[A](as: Lst[A])(f: A => Boolean): Lst[A] = foldRight(as, Nill: Lst[A])((a, b) => if (f(a)) Conss(a, b) else b)
  
  def flatMap[A, B](as: Lst[A])(f: A => Lst[B]): Lst[B] = foldRight(as, Nill: Lst[B])((a, b) => append(f(a), b))
  
  
}