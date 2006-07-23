{- Depending how much memory you have, you may need to reduce 'nine' -}

main () -> Unit = 
    print(natToInt(fact(ten)))

readStr () -> String =
    foreign String "readStr" ()


natrec (x:Data, z:Any, s:Fun) -> Any = natrectail(x,s,z)

natrectail (x:Data, s:Fun, acc:Data) -> Any =
  case x of {
     Con 0 () -> acc
   | Con 1 (k:Data) -> natrectail(k,s,s(k,acc))
  }

zero () -> Data = Con 0 ()
one () -> Data = Con 1 (zero)
two () -> Data = Con 1 (one)
three () -> Data = Con 1 (two)
four () -> Data = Con 1 (three)
five () -> Data = Con 1 (four)
six () -> Data = Con 1 (five)
seven () -> Data = Con 1 (six)
eight () -> Data = Con 1 (seven)
nine () -> Data = Con 1 (eight)
ten () -> Data = Con 1 (nine)

print (x:Int) -> Unit =
  let foo:Unit = foreign Unit "printInt" (x:Int) in unit

{-
natToInt (x:Data) -> Int =
  case x of {
     Con 0 () -> 0
   | Con 1 (k:Data) -> 1+natToInt(k)
  }
-}

natToInt (x:Data) -> Int =
    natrec(x,0,n2isuc)

n2isuc (k:Data, ih:Int) -> Int = 1+ih

plus (x:Data, y:Data) -> Data =  
    natrec(x,y,plussuc)

plussuc (k:Data, ih:Data) -> Data = Con 1 (ih)

mult (x:Data, y:Data) -> Data = 
  natrec(x, Con 0 (), multsuc(y))

multsuc (y:Data, k:Data, ih:Data) -> Data = plus(y, ih)

fact (x:Data) -> Data =
  case x of {
     Con 0 () -> one()
   | Con 1 (k:Data) -> mult(x, fact(k))
  }

