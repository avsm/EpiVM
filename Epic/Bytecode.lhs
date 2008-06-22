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
>             | IF TmpVar Bytecode Bytecode
>             | OP TmpVar Op TmpVar TmpVar
>             | LOCALS Int -- allocate space for locals
>             | TMPS Int -- declare temporary variables
>             | EVAL TmpVar
>             -- | LET TmpVar Local TmpVar
>             | RETURN TmpVar
>             | ERROR String -- Fatal error, exit
>   deriving Show

> type Bytecode = [ByteOp]

> data FunCode = Code [Type] Bytecode
>   deriving Show

> data CompileState = CS { arg_types :: [Type],
>                          num_locals :: Int,
>                          next_tmp :: Int }

> compile :: Context -> Func -> FunCode
> compile ctxt fn@(Bind args locals def) = 
>     let cs = (CS (map snd args) (length args) 1)
>         code = evalState (scompile ctxt fn) cs in
>         Code (map snd args) code

> data TailCall = Tail | Middle

> scompile :: Context -> Func -> State CompileState Bytecode
> scompile ctxt (Bind args locals def) = 
>     do -- put (CS args (length args) 1)
>        code <- ecomp Tail def 0 (length args)
>        cs <- get
>        return $ (LOCALS (num_locals cs)):(TMPS (next_tmp cs)):code++[RETURN 0]

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

>     ecomp :: TailCall -> Expr -> TmpVar -> Int -> 
>              State CompileState Bytecode
>     ecomp tcall (V v) reg vs = 
>         do return [VAR reg v]
>     ecomp tcall (R x) reg vs = acomp tcall False (R x) [] reg vs
>     ecomp tcall (App f as) reg vs = acomp tcall False f as reg vs
>     ecomp tcall (LazyApp f as) reg vs = acomp tcall True f as reg vs
>     ecomp tcall (Con t as) reg vs = 
>         do (argcode, argregs) <- ecomps as vs
>            return $ argcode ++ [CON reg t argregs]
>     ecomp tcall (Proj con i) reg vs =
>         do reg' <- new_tmp
>            concode <- ecomp Middle con reg' vs
>            return [PROJ reg reg' i]
>     ecomp tcall (Const c) reg vs = ccomp c reg
>     ecomp tcall (Case scrutinee alts) reg vs =
>         do screg <- new_tmp
>            sccode <- ecomp Middle scrutinee screg vs
>            (altcode, def) <- altcomps tcall (order alts) screg reg vs
>            return $ sccode ++ [EVAL screg, CASE screg altcode def]
>     ecomp tcall (If a t e) reg vs =
>         do areg <- new_tmp
>            acode <- ecomp Middle a areg vs
>            tcode <- ecomp tcall t reg vs
>            ecode <- ecomp tcall e reg vs
>            return $ acode ++ [EVAL areg, IF areg tcode ecode]
>     ecomp tcall (Op op l r) reg vs =
>         do lreg <- new_tmp
>            rreg <- new_tmp
>            lcode <- ecomp Middle l lreg vs
>            rcode <- ecomp Middle r rreg vs
>            return $ lcode ++ [EVAL lreg] ++ 
>                     rcode ++ [EVAL rreg, OP reg op lreg rreg]
>     ecomp tcall (Let nm ty val scope) reg vs =
>         do loc <- new_locals 1
>            reg' <- new_tmp
>            valcode <- ecomp Middle val reg' vs
>            scopecode <- ecomp tcall scope reg vs
>            return $ valcode ++ (ASSIGN loc reg'):scopecode
>     ecomp tcall (Error str) reg vs = return [ERROR str]
>     ecomp tcall Impossible reg vs = return [ERROR "The impossible happened."]
>     ecomp tcall (ForeignCall ty fn argtypes) reg vs = do
>           let (args,types) = unzip argtypes
>           (argcode, argregs) <- ecomps args vs
>           let evalcode = map EVAL argregs
>           return $ argcode ++ evalcode ++ [FOREIGN ty reg fn (zip argregs types)]

>     ecomps :: [Expr] -> Int -> State CompileState (Bytecode, [TmpVar])
>     ecomps e vs = ecomps' [] [] e vs
>     ecomps' code tmps [] vs = return (code, tmps)
>     ecomps' code tmps (e:es) vs =
>         do reg <- new_tmp
>            ecode <- ecomp Middle e reg vs
>            ecomps' (code++ecode) (tmps++[reg]) es vs

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

>     altcomps :: TailCall -> [CaseAlt] -> TmpVar -> TmpVar -> Int ->
>                 State CompileState ([(Int, Bytecode)], Maybe Bytecode)
>     altcomps tc [] _ _ vs = return ([], Nothing)
>     altcomps tc (a:as) scrutinee reg vs = 
>         do (t,acode) <- altcomp tc a scrutinee reg vs
>            (ascode, def) <- altcomps tc as scrutinee reg vs
>            if (t<0) then return (ascode, Just acode)
>                     else return ((t,acode):ascode, def)

Assume that all the tags are in order, and unused constructors have 
a default inserted (i.e., tag can be ignored).

Return the tag and the code - tag is -1 for default case.

>     altcomp :: TailCall -> CaseAlt -> TmpVar -> TmpVar -> Int ->
>                State CompileState (Int, Bytecode)
>     altcomp tc (Alt tag nmargs expr) scrutinee reg vs =
>         do let args = map snd nmargs
>            local <- new_locals (length args)
>            projcode <- project args scrutinee vs 0
>            exprcode <- ecomp tc expr reg (vs+(length args))
>            return (tag, projcode++exprcode)
>     altcomp tc (DefaultCase expr) scrutinee reg vs =
>         do exprcode <- ecomp tc expr reg vs
>            return (-1,exprcode)

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
>               do (argcode, argregs) <- ecomps args vs
>                  return $ argcode ++ map EVAL argregs ++ [(tcall tc) reg x argregs]
>           | otherwise =
>               do (argcode, argregs) <- ecomps args vs
>                  return $ argcode ++ [THUNK reg (arity x ctxt) x argregs]
>      where tcall Tail = TAILCALL
>            tcall Middle = CALL
>     acomp _ _ f args reg vs
>           = do (argcode, argregs) <- ecomps args vs
>                reg' <- new_tmp
>                fcode <- ecomp Middle f reg' vs
>                return $ fcode ++ argcode ++ [ADDARGS reg reg' argregs]

>     ccomp (MkInt i) reg = return [INT reg i]
>     ccomp (MkBigInt i) reg = return [BIGINT reg i]
>     ccomp (MkChar c) reg = return [INT reg (fromEnum c)]
>     ccomp (MkFloat f) reg = return [FLOAT reg f]
>     ccomp (MkBigFloat f) reg = return [BIGFLOAT reg f]
>     ccomp (MkBool b) reg = return [INT reg (if b then 1 else 0)]
>     ccomp (MkString s) reg = return [STRING reg s]
>     ccomp (MkUnit) reg = return [UNIT reg]

