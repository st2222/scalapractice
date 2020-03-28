

sealed trait Tree[+A]{
  def size(): Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size() + r.size()
  }
  
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }
  
}
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree{
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }
}