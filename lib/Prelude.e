%include "string.h"

-- IO

putStr (x:String) -> Unit =
    foreign Unit "putStr" (x:String)

putStrLn (x:String) -> Unit =
    putStr(append(x,"\n"))

readStr () -> String =
    foreign String "readStr" ()

intToStr (x:Int) -> String =
    foreign String "intToStr" (x:Int)

printInt (x:Int) -> Unit =
  let foo:Unit = foreign Unit "printInt" (x:Int) in unit

-- String operations

append (x:String, y:String) -> String =
    foreign String "append" (x:String, y:String)

length (x:String) -> String =
    foreign Int "strlen" (x:String)

-- Big number arithmetic

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

