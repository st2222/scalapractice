
trait RNG {
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
  type Rand[+A] = RNG => (A, RNG)
  
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if(i < 0) -(i + 1) else i, r)
  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0){
      (List(), rng)
    }else{
      val (i, r) = rng.nextInt
      val (i2, r2) = ints(count -1)(r)
      (i :: i2, r2)
    }
  }
  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C):Rand[C] = {
    rng => {
      val (ia, rra) = ra(rng)
      val (ib, rrb) = rb(rra)
      (f(ia, ib), rrb)
    }
  }
}
