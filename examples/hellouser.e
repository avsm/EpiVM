main () -> Unit = 
    putStr("What is your name? ");
    let name:String = readStr() in
    putStrLn(append("Hello ",name));
    putStrLn(append("Your name is ",append(intToStr(length(name)),
					   " letters long")))
