main () -> Unit = putStrLn(bigIntToStr(fact(10000L)))

fact (x:BigInt) -> BigInt = factAux(x,1L)

factAux (x:BigInt, acc:BigInt) -> BigInt = 
     if (eqBig(x,0L)) 
       then acc
       else factAux(subBig(x,1L), mulBig(x,acc))

