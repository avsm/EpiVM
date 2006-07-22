{- Depending how much memory you have, you may need to reduce 'nine' -}

main () -> Unit = 
  print(natToInt(fact(nine))) 

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

natToInt(x:Data) -> Int = auxnatToInt(x,0)

auxnatToInt (x:Data, acc:Int) -> Int =
  case x of {
     Con 0 () -> acc
   | Con 1 (k:Data) -> auxnatToInt(k,1+acc)
  }

plus (x:Data, y:Data) -> Data = 
  case x of {
     Con 0 () -> y
   | Con 1 (k:Data) -> plus(k, Con 1 (y))
  }

mult(x:Data,y:Data) -> Data = auxmult(x,y,Con 0 ())

auxmult (x:Data, y:Data, acc:Data) -> Data = 
  case x of {
     Con 0 () -> acc
   | Con 1 (k:Data) -> auxmult(k, y, plus(y, acc))
  }

fact (x:Data) -> Data =
  case x of {
     Con 0 () -> one()
   | Con 1 (k:Data) -> mult(x, fact(k))
  }

apply (f:Fun, a:Any) -> Any =
  f(a)

