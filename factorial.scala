def fact(x: BigInt): BigInt = 
  var f: BigInt = 1
  var y = x
  while y > 1 do
    f = f * y
    y = y - 1
  f
  
@main def m(args: String*) =
  args.foreach(x => println(x + "! = " + fact(new java.math.BigInteger(x))))
