// Write a function to find the longest common prefix string amongst an array of strings.
// If there is no common prefix, return an empty string ""
object leetCode14:
  def longestCommonPrefix(strs: Array[String]): String =
    def isPrefixInStings(s: String) = strs.forall(_.startsWith(s))
    val minLengthString = strs.min(Ordering.by(_.length))
    if minLengthString.nonEmpty then
      var prefix = minLengthString.head.toString
      for position <- 0 to minLengthString.length do
        val prfStr = minLengthString.substring(0, position)
        if isPrefixInStings(prfStr) then
          prefix = prfStr
        else
          return prefix
      minLengthString
    else
      ""

  def main(args: Array[String]): Unit =

    assert(longestCommonPrefix(Array("flower", "flow", "flight")) == "fl")
    assert(longestCommonPrefix(Array("dog", "racecar", "car")) == "")

    //println(s"${longestCommonPrefix(args)}")
