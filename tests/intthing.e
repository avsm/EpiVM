include "Prelude.e"

main () -> Unit =
    printInt(foo(9))

foo (x:Int) -> Int =
    if x<=0 then 1 else x*foo(x-1)
