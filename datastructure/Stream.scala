//import scala.{ Option => _, Either => _, Stream => _}
import Stream._
sealed trait Stream[+A] {
  def toList(): List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: l)
      case _          => l
    }
    go(this, Nil).reverse
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1)  => t().drop(n - 1)
    case Cons(h, t) if (n == 1) => t()
    case _                      => empty
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty      => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def forAll(f: A => Boolean): Boolean = {
    foldRight(true)((a, b) => f(a) && b)
  }
  def takeWhile(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty)
  }
  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((x, y) => cons(x, y))
  }
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }
  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)
  }
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a) append b)
  }

}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => t)
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))
  }

  def empty[A]: Stream[A] = Empty
  def constant[A](a: A): Stream[A] = {
    lazy val constant: Stream[A] = Cons(() => a, () => constant)
    constant
  }
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }
  val fibs = {
    def go(f1: Int, f2: Int): Stream[Int] = {
      cons(f1, go(f2, f2 + f1))
    }
    go(0, 1)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, t)) => cons(h, unfold(z)(f))
      case None => empty
    }
  }
  val fibsViaUnfold = unfold((0, 1)){ case (f0, f1) => Some((f0, (f1, f0 + f1))) }
}

//
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
//

