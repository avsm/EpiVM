main () -> Unit = printBig(fact(120L))

fact (x:BigInt) -> BigInt = 
     if (eqBig(x,0L)) 
       then 1L
       else mulBig(x,fact(subBig(x,1L)))

addBig (x:BigInt, y:BigInt) -> BigInt =
   foreign BigInt "addBigInt" (x:BigInt, y:BigInt)

subBig (x:BigInt, y:BigInt) -> BigInt =
   foreign BigInt "subBigInt" (x:BigInt, y:BigInt)

mulBig (x:BigInt, y:BigInt) -> BigInt =
   foreign BigInt "mulBigInt" (x:BigInt, y:BigInt)

eqBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "eqBigInt" (x:BigInt, y:BigInt)

printBig (x:BigInt) -> Unit =
   foreign Unit "printBigInt" (x:BigInt)