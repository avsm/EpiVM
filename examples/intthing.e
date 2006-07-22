print (x:Int) -> Unit =
  let foo:Unit = foreign Unit "printInt" (x:Int) in unit

main () -> Unit =
    print(foo(9))

foo (x:Int) -> Int =
    if x<=0 then 1 else x*foo(x-1)
