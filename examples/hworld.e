putStr (x:String) -> Unit =
    foreign Unit "putStr" (x:String);

main () -> Unit = 
    putStr("Hello world!\n");


