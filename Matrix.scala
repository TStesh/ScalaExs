class Matrix(a: BigInt, b: BigInt, c: BigInt, d: BigInt):

  val a00: BigInt = a
  val a01: BigInt = b
  val a10: BigInt = c
  val a11: BigInt = d

  override def toString: String = s"\n| $a00 $a01 |\n| $a10 $a11 |"

  def + (that: Matrix): Matrix = Matrix(
    a00 + that.a00, a01 + that.a01,
    a10 + that.a10, a11 + that.a11
  )

  def * (that: Matrix): Matrix = Matrix (
    a00 * that.a00 + a01 * that.a10, a00 * that.a01 + a01 * that.a11,
    a10 * that.a00 + a11 * that.a10, a10 * that.a01 + a11 * that.a11
  )

object Matrix:

  private val one = Matrix(1, 0, 0, 1)

  private def pow(x: Matrix, y: Int): Matrix =
    // локальное замыкание
    def odd: Boolean = (y & 1) == 1
    // основная логика
    y match
      case 0 => one
      case 1 => x
      case _ =>
        val t = pow(x * x, y >> 1)
        if odd then t * x else t

  def fib(n: Int): BigInt = pow(Matrix(1, 1, 1, 0), n).a10

  def main(args: Array[String]): Unit =
    for arg <- args do
      println(s"$arg: ${fib(arg.toInt)}")

