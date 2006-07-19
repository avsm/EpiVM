main () -> Unit = 
  print(natToInt(fact(nine)));

zero () -> Data = Con 0 ();
one () -> Data = Con 1 (zero);
two () -> Data = Con 1 (one);
three () -> Data = Con 1 (two);
four () -> Data = Con 1 (three);
five () -> Data = Con 1 (four);
six () -> Data = Con 1 (five);
seven () -> Data = Con 1 (six);
eight () -> Data = Con 1 (seven);
nine () -> Data = Con 1 (eight);
ten () -> Data = Con 1 (nine);

print (x:Int) -> Unit =
  let foo:Unit = foreign Unit "printInt" (x:Int) in unit;

natToInt (x:Data) -> Int =
  case x of {
     Con 0 () -> 0
   | Con 1 (k:Data) -> 1+natToInt(k)
  };

plus (x:Data, y:Data) -> Data = 
  case x of {
     Con 0 () -> y
   | Con 1 (k:Data) -> Con 1 (plus(k, y))
  };

mult (x:Data, y:Data) -> Data = 
  case x of {
     Con 0 () -> Con 0 ()
   | Con 1 (k:Data) -> plus(y, (mult(k, y)))
  };

fact (x:Data) -> Data =
  case x of {
     Con 0 () -> one()
   | Con 1 (k:Data) -> mult(x, fact(k))
  };

apply (f:Fun, a:Any) -> Any =
  f(a);

