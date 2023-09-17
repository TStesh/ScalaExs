// Простейшее решето
object sieveForPrimeNumbers:
  def isNum(s: String): Boolean =
    s.toIntOption.isDefined

  def sieve(num: Int): Array[Int] =
    val arr = (0 to num).toArray[Int]
    for
      x <- arr.drop(2)
      x2 = x * x
      if x2 <= num && x > 0
      y <- x2.to(num, x)
    do arr(y) = 0
    arr.drop(2)

  def main(args: Array[String]): Unit =
    if args.length == 1 && isNum(args(0)) then
      val primes = sieve(args(0).toInt).filter(_ > 0)
      println(s"${primes.mkString(", ")}")
      println(s"#${args(0)} = ${primes.length}")
    else
      println("Usage: scala sieve.scala NUMBER")