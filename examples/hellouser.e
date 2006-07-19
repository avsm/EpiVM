%include "string.h"

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

main () -> Unit = 
    let foo:Unit = putStr("What is your name? ") in
    let name:String = readStr() in
    let foo:Unit = putStrLn(append("Hello ",name)) in
    putStrLn(append("Your name is ",append(intToStr(length(name)),
					   " letters long")));
