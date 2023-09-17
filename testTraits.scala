def palyndromic(x: String): Boolean =
  val n = x.length
  if n < 2 then return true
  val m = n >> 1
  (0 until m).takeWhile(i => x.charAt(i) == x.charAt(n - i - 1)).length == m

def sumDigits(x: Long): Int =
  x.toString.foldLeft(0)(_ + _.toInt - 48)

def sieve(num: Int): Array[Int] =
  val arr = (0 to num).toArray[Int]
  for
    x <- arr.drop(2)
    x2 = x * x
    if x2 <= num && x > 0
    y <- x2.to(num, x)
  do arr(y) = 0
  arr.drop(2)

@main def f(): Unit =
  // println(sumDigits(123456789))
  // val a = (0 to 10).map(_ => 0)
  // println(a)

  // Решето
  val bs = scala.collection.mutable.BitSet.empty
  val n = 100
  val id = (x: Int) => (x - 1) >> 1
  3.to(n, 2).takeWhile(i => i * i < n).foreach(x =>
    if !bs(id(x)) then (3 * x).to(n, x << 1).foreach(bs += id(_))
  )
  val primes = 3.to(n, 2).filter(x => !bs(id(x)))
  println(primes)
