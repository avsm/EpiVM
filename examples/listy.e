%include "string.h"

main () -> Unit = let foo:Unit = printList(take(3,ones)) in unit;

take (i:Int, x:Data) -> Data
  = if (i==0) then Con 0 () else
	case x of {
	   Con 0 () -> Con 0 ()
         | Con 1 (y:Any,ys:Data) -> Con 1 (y, take(i-1, ys))
        };

testList () -> Data
  = Con 1 (1, Con 1 (2, Con 1 (3, Con 1 (4, Con 1 (5, Con 0 ())))));

ones () -> Data
  = Con 1 (1, lazy(ones)); -- needs to be lazy or it runs forever!

{- IO stuff -}

printList (x:Data) -> Data
  = case x of {
        Con 1 (y:Int, ys:Data) -> 
	   let foo:Unit = putStr(append(intToStr(y),", ")) in
	   printList(ys)
      | Con 0 () -> putStrLn("nil")
    };

putStr (x:String) -> Unit =
    foreign Unit "putStr" (x:String);

putStrLn (x:String) -> Unit =
    putStr(append(x,"\n"));

readStr () -> String =
    foreign String "readStr" ();

append (x:String, y:String) -> String =
    foreign String "append" (x:String, y:String);

length (x:String) -> String =
    foreign Int "strlen" (x:String);

intToStr (x:Int) -> String =
    foreign String "intToStr" (x:Int);

