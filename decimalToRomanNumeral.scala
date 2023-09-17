// Перевод из десятичной системы счисления в римскую
object decimalToRomanNumeral:
  private val romanNumeral: Map[Int, String] = Map(
    1 -> "I", 4 -> "IV", 5 -> "V", 9 -> "IX", 10 -> "X",
    40 -> "XL", 50 -> "L", 90 -> "XC", 100 -> "C",
    400 -> "CD", 500 -> "D", 900 -> "CM", 1000 -> "M"
  )

  private def conv(num: Int, d: Int): String =
    if romanNumeral.contains(num) then romanNumeral(num)
    else conv(num - d, d) + romanNumeral(d)

  private def convNum(s: String): String =
    s.reverse.map(_.toInt - 48)
      .zipWithIndex
      .reverse.map((a, b) =>
      val tmp = math.pow(10, b).toInt
        conv(a * tmp, tmp)
    )
    .mkString("")

  private def convert(s: String): String =
    def isNum: Boolean = s.toIntOption.isDefined

    val rs = if isNum then
      if s.toInt < 4000 then
        val ns = s.toInt
        if romanNumeral.contains(ns) then
          romanNumeral(ns)
        else
          convNum(s)
      else
        "too big (>3 999)"
    else
      "is not a number"
    rs
  
  def main(args: Array[String]): Unit =
    for arg <- args do
      println(s"$arg: ${convert(arg)}")
