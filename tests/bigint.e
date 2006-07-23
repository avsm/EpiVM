include "Prelude.e"

main () -> Unit = printBig(fact(120L))

fact (x:BigInt) -> BigInt = 
     if (eqBig(x,0L)) 
       then 1L
       else mulBig(x,fact(subBig(x,1L)))

