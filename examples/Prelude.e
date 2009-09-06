%include "string.h"

-- IO

%inline putStr (x:String) -> Unit =
    foreign Unit "putStr" (x:String)

putStrLn (x:String) -> Unit =
    putStr(append(x,"\n"))

readStr () -> String =
    foreign String "readStr" ()

intToStr (x:Int) -> String =
    foreign String "intToStr" (x:Int)

strToInt (x:String) -> Int =
    foreign String "strToInt" (x:String)

printInt (x:Int) -> Unit =
    foreign Unit "printInt" (x:Int)

-- String operations

append (x:String, y:String) -> String =
    foreign String "append" (x:String, y:String)

length (x:String) -> String =
    foreign Int "strlen" (x:String)

index (x:String, i:Int) -> Char =
    foreign Int "strIndex" (x:String, i:Int)

-- Big number arithmetic

addBig (x:BigInt, y:BigInt) -> BigInt =
   foreign BigInt "addBigInt" (x:BigInt, y:BigInt)

subBig (x:BigInt, y:BigInt) -> BigInt =
   foreign BigInt "subBigInt" (x:BigInt, y:BigInt)

mulBig (x:BigInt, y:BigInt) -> BigInt =
   foreign BigInt "mulBigInt" (x:BigInt, y:BigInt)

eqBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "eqBigInt" (x:BigInt, y:BigInt)

ltBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "ltBigInt" (x:BigInt, y:BigInt)

gtBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "gtBigInt" (x:BigInt, y:BigInt)

leBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "leBigInt" (x:BigInt, y:BigInt)

geBig (x:BigInt, y:BigInt) -> Bool =
   foreign Int "geBigInt" (x:BigInt, y:BigInt)

printBig (x:BigInt) -> Unit =
   foreign Unit "printBigInt" (x:BigInt)

bigIntToStr (x:BigInt) -> String =
    foreign String "bigIntToStr" (x:BigInt)

strToBigInt (x:String) -> Int =
    foreign String "strToBigInt" (x:String)

