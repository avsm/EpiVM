> module Epic.Bytecode where

> import Control.Monad.State
> import List

> import Epic.Language

> type Local = Int
> type TmpVar = Int

Register based - most operations do an action, then put the result in a
'TmpVar' which is basically a numbered register. There are infinite registers
at this stage.

> data ByteOp = CALL TmpVar Name [TmpVar]
>             | TAILCALL TmpVar Name [TmpVar]
>             | THUNK TmpVar Int Name [TmpVar]
>             | ADDARGS TmpVar TmpVar [TmpVar]
>             | FOREIGN Type TmpVar String [(TmpVar, Type)]
>             | VAR TmpVar Local
>             | ASSIGN Local TmpVar
>             | CON TmpVar Tag [TmpVar]
>             | UNIT TmpVar
>             | INT TmpVar Int
>             | BIGINT TmpVar Integer
>             | FLOAT TmpVar Float
>             | BIGFLOAT TmpVar Double
>             | STRING TmpVar String
>             | PROJ TmpVar TmpVar Int -- project into a register
>             | PROJVAR Local TmpVar Int -- project into a local variable
>               -- each case branch records which tag it's code for
>             | CASE TmpVar [(Int, Bytecode)] (Maybe Bytecode)
>             | INTCASE TmpVar [(Int, Bytecode)] (Maybe Bytecode)
>             | IF TmpVar Bytecode Bytecode
>             | OP TmpVar Op TmpVar TmpVar
>             | LOCALS Int -- allocate space for locals
>             | TMPS Int -- declare temporary variables
>             | EVAL TmpVar
>             -- | LET TmpVar Local TmpVar
>             | RETURN TmpVar
>             | DRETURN -- return dummy value
>             | ERROR String -- Fatal error, exit
>             | TRACE String [TmpVar]
>   deriving Show

> type Bytecode = [ByteOp]

> data FunCode = Code [Type] Bytecode
>   deriving Show

> data CompileState = CS { arg_types :: [Type],
>                          num_locals :: Int,
>                          next_tmp :: Int }

> compile :: Context -> Name -> Func -> FunCode
> compile ctxt fname fn@(Bind args locals def) = 
>     let cs = (CS (map snd args) (length args) 1)
>         code = evalState (scompile ctxt fname fn) cs in
>         Code (map snd args) code

> data TailCall = Tail | Middle

> scompile :: Context -> Name -> Func -> State CompileState Bytecode
> scompile ctxt fname (Bind args locals def) = 
>     do -- put (CS args (length args) 1)
>        code <- ecomp False Tail def 0 (length args)
>        cs <- get
>        return $ (LOCALS (num_locals cs)):
>                 (TRACE (show fname) [0..(length args)-1]):
>                 (TMPS (next_tmp cs)):code ++[EVAL 0, RETURN 0]

>   where

>     new_tmp :: State CompileState Int
>     new_tmp = do cs <- get
>                  let reg' = next_tmp cs
>                  put (cs { next_tmp = reg'+1 } )
>                  return reg'

Add some locals, return de Bruijn level of first new one.

>     new_locals :: Int -> State CompileState Int
>     new_locals args = 
>         do cs <- get
>            let loc = num_locals cs
>            put (cs { num_locals = loc+args } )
>            return loc

Take an expression and the register (TmpVar) to put the result into;
compile code to do just that.

Also carry the number of real variables currently in scope so that, in 
particular, when we project from a data structure we store it in the right
place.

>     ecomp :: Bool -> TailCall -> Expr -> TmpVar -> Int -> 
>              State CompileState Bytecode
>     ecomp lazy tcall (V v) reg vs = 
>         do return [VAR reg v]
>     ecomp lazy tcall (R x) reg vs = acomp tcall lazy (R x) [] reg vs
>     ecomp lazy tcall (App f as) reg vs = acomp tcall lazy f as reg vs
>     ecomp lazy tcall (Lazy e) reg vs = ecomp True tcall e reg vs
>     ecomp lazy tcall (Con t as) reg vs = 
>         do (argcode, argregs) <- ecomps lazy as vs
>            return $ argcode ++ [CON reg t argregs]
>     ecomp lazy tcall (Proj con i) reg vs =
>         do reg' <- new_tmp
>            concode <- ecomp lazy Middle con reg' vs
>            return $ concode ++ [EVAL reg', PROJ reg reg' i]
>     ecomp lazy tcall (Const c) reg vs = ccomp c reg
>     ecomp lazy tcall (Case scrutinee alts) reg vs =
>         do screg <- new_tmp
>            sccode <- ecomp lazy Middle scrutinee screg vs
>            (altcode, def) <- altcomps lazy tcall (order alts) screg reg vs
>            return $ sccode ++ [EVAL screg, (caseop alts) screg altcode def]
>     ecomp lazy tcall (If a t e) reg vs =
>         do areg <- new_tmp
>            acode <- ecomp lazy Middle a areg vs
>            tcode <- ecomp lazy tcall t reg vs
>            ecode <- ecomp lazy tcall e reg vs
>            return $ acode ++ [EVAL areg, IF areg tcode ecode]
>     ecomp lazy tcall (Op op l r) reg vs =
>         do lreg <- new_tmp
>            rreg <- new_tmp
>            lcode <- ecomp lazy Middle l lreg vs
>            rcode <- ecomp lazy Middle r rreg vs
>            return $ lcode ++ [EVAL lreg] ++ 
>                     rcode ++ [EVAL rreg, OP reg op lreg rreg]
>     ecomp lazy tcall (Let nm ty val scope) reg vs =
>         do loc <- new_locals 1
>            reg' <- new_tmp
>            valcode <- ecomp lazy Middle val reg' vs
>            scopecode <- ecomp lazy tcall scope reg (vs+1)
>            return $ valcode ++ (EVAL reg'):(ASSIGN vs reg'):scopecode
>     ecomp lazy tcall (Error str) reg vs = return [ERROR str]
>     ecomp lazy tcall Impossible reg vs = return [ERROR "The impossible happened."]
>     ecomp lazy tcall (ForeignCall ty fn argtypes) reg vs = do
>           let (args,types) = unzip argtypes
>           (argcode, argregs) <- ecomps lazy args vs
>           let evalcode = map EVAL argregs
>           return $ argcode ++ evalcode ++ [FOREIGN ty reg fn (zip argregs types)]
>     ecomp lazy tcall (LazyForeignCall ty fn argtypes) reg vs = do
>           let (args,types) = unzip argtypes
>           (argcode, argregs) <- ecomps lazy args vs
>           return $ argcode ++ [FOREIGN ty reg fn (zip argregs types)]

>     ecomps :: Bool -> [Expr] -> Int -> State CompileState (Bytecode, [TmpVar])
>     ecomps lazy e vs = ecomps' lazy [] [] e vs
>     ecomps' lazy code tmps [] vs = return (code, tmps)
>     ecomps' lazy code tmps (e:es) vs =
>         do reg <- new_tmp
>            ecode <- ecomp lazy Middle e reg vs
>            ecomps' lazy (code++ecode) (tmps++[reg]) es vs

Compile case alternatives.

>     order :: [CaseAlt] -> [CaseAlt]
>     order xs = sort xs -- insertError 0 (sort xs)

We don't do this any more, now that we have default cases.

>     insertError t [] = []
>     insertError t (a@(Alt tn _ _):xs)
>         = {- (errors t tn) ++ -} a:(insertError (tn+1) xs)
>     insertError t rest@((DefaultCase _):xs)
>         = rest -- End at defaul case

>     errors x end | x == end = []
>                  | otherwise = (Alt x [] Impossible):(errors (x+1) end)

>     altcomps :: Bool -> TailCall -> [CaseAlt] -> TmpVar -> TmpVar -> Int ->
>                 State CompileState ([(Int, Bytecode)], Maybe Bytecode)
>     altcomps lazy tc [] _ _ vs = return ([], Nothing)
>     altcomps lazy tc (a:as) scrutinee reg vs = 
>         do (t,acode) <- altcomp lazy tc a scrutinee reg vs
>            (ascode, def) <- altcomps lazy tc as scrutinee reg vs
>            if (t<0) then return (ascode, Just acode)
>                     else return ((t,acode):ascode, def)

Assume that all the tags are in order, and unused constructors have 
a default inserted (i.e., tag can be ignored).

Return the tag and the code - tag is -1 for default case.

>     altcomp :: Bool -> TailCall -> CaseAlt -> TmpVar -> TmpVar -> Int ->
>                State CompileState (Int, Bytecode)
>     altcomp lazy tc (Alt tag nmargs expr) scrutinee reg vs =
>         do let args = map snd nmargs
>            local <- new_locals (length args)
>            projcode <- project args scrutinee vs 0
>            exprcode <- ecomp lazy tc expr reg (vs+(length args))
>            return (tag, projcode++exprcode)
>     altcomp lazy tc (ConstAlt tag expr) scrutinee reg vs =
>         do exprcode <- ecomp lazy tc expr reg (vs+(length args))
>            return (tag, exprcode)
>     altcomp lazy tc (DefaultCase expr) scrutinee reg vs =
>         do exprcode <- ecomp lazy tc expr reg vs
>            return (-1,exprcode)

>     caseop ((ConstAlt _ _):_) = INTCASE
>     caseop _ = CASE

>     project [] _ _ _ = return []
>     project (_:as) scr loc arg = 
>         do let acode = PROJVAR loc scr arg
>            ascode <- project as scr (loc+1) (arg+1)
>            return (acode:ascode)

Compile an application of a function to arguments

>     acomp :: TailCall -> Bool -> Expr -> [Expr] -> TmpVar -> Int ->
>              State CompileState Bytecode
>     acomp tc lazy (R x) args reg vs
>           | lazy == False && arity x ctxt == length args =
>               do (argcode, argregs) <- ecomps lazy args vs
>                  return $ argcode {- ++ map EVAL argregs -} ++ [(tcall tc) reg x argregs]
>           | otherwise =
>               do (argcode, argregs) <- ecomps lazy args vs
>                  return $ argcode ++ [THUNK reg (arity x ctxt) x argregs] ++ 
>                           if (not lazy) then [EVAL reg] else []
>      where tcall Tail = TAILCALL
>            tcall Middle = CALL
>     acomp _ lazy f args reg vs
>           = do (argcode, argregs) <- ecomps lazy args vs
>                reg' <- new_tmp
>                fcode <- ecomp lazy Middle f reg' vs
>                return $ fcode ++ argcode ++ [ADDARGS reg reg' argregs]

>     ccomp (MkInt i) reg = return [INT reg i]
>     ccomp (MkBigInt i) reg = return [BIGINT reg i]
>     ccomp (MkChar c) reg = return [INT reg (fromEnum c)]
>     ccomp (MkFloat f) reg = return [FLOAT reg f]
>     ccomp (MkBigFloat f) reg = return [BIGFLOAT reg f]
>     ccomp (MkBool b) reg = return [INT reg (if b then 1 else 0)]
>     ccomp (MkString s) reg = return [STRING reg s]
>     ccomp (MkUnit) reg = return [UNIT reg]

