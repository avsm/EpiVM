include "Prelude.e"

main () -> Unit = 
  printInt(natToInt(adder(three, two, three, four, five)))

%inline adder (arity:Data, acc:Data) -> Any =
  case arity of {
     Con 0 () -> acc
   | Con 1 (k:Data) -> adderAux(k, acc)
  }

adderAux (k:Data, acc:Data, n:Data) -> Any =
  adder(k,plus(acc,n))

adderAuxE (k:Data, acc:Data, n:Data) -> Any =
  case k of {
     Con 0 () -> plus(acc,n)
   | Con 1 (k:Data) -> adderAuxE(k,plus(acc,n))
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

natToInt (x:Data) -> Int =
  case x of {
     Con 0 () -> 0
   | Con 1 (k:Data) -> 1+natToInt(k)
  }

plus (x:Data, y:Data) -> Data = 
  case x of {
     Con 0 () -> y
   | Con 1 (k:Data) -> Con 1 (plus(k, y))
  }

