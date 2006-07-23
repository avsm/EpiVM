> module EMachine.Bytecode where

> import Control.Monad.State
> import List

> import EMachine.Language

> type Local = Int
> type Tag = Int
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
>             | CASE TmpVar [Bytecode]
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
>        code <- ecomp Tail def 0
>        cs <- get
>        return $ (LOCALS locals):(TMPS (next_tmp cs)):code++[RETURN 0]

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

>     ecomp :: TailCall -> Expr -> TmpVar -> State CompileState Bytecode
>     ecomp tcall (V v) reg = 
>         do return [VAR reg v]
>     ecomp tcall (R x) reg = acomp tcall False (R x) [] reg 
>     ecomp tcall (App f as) reg = acomp tcall False f as reg
>     ecomp tcall (LazyApp f as) reg = acomp tcall True f as reg
>     ecomp tcall (Con t as) reg = 
>         do (argcode, argregs) <- ecomps as
>            return $ argcode ++ [CON reg t argregs]
>     ecomp tcall (Proj con i) reg =
>         do reg' <- new_tmp
>            concode <- ecomp Middle con reg'
>            return [PROJ reg reg' i]
>     ecomp tcall (Const c) reg = ccomp c reg
>     ecomp tcall (Case scrutinee alts) reg =
>         do screg <- new_tmp
>            sccode <- ecomp Middle scrutinee screg
>            altcode <- altcomps tcall (order alts) screg reg
>            return $ sccode ++ [EVAL screg, CASE screg altcode]
>     ecomp tcall (If a t e) reg =
>         do areg <- new_tmp
>            acode <- ecomp Middle a areg
>            tcode <- ecomp tcall t reg
>            ecode <- ecomp tcall e reg
>            return $ acode ++ [EVAL areg, IF areg tcode ecode]
>     ecomp tcall (Op op l r) reg =
>         do lreg <- new_tmp
>            rreg <- new_tmp
>            lcode <- ecomp Middle l lreg
>            rcode <- ecomp Middle r rreg
>            return $ lcode ++ [EVAL lreg] ++ 
>                     rcode ++ [EVAL rreg, OP reg op lreg rreg]
>     ecomp tcall (Let nm ty val scope) reg =
>         do loc <- new_locals 1
>            reg' <- new_tmp
>            valcode <- ecomp Middle val reg'
>            scopecode <- ecomp tcall scope reg
>            return $ valcode ++ (ASSIGN loc reg'):scopecode
>     ecomp tcall (Error str) reg = return [ERROR str]
>     ecomp tcall Impossible reg = return [ERROR "The impossible happened."]
>     ecomp tcall (ForeignCall ty fn argtypes) reg = do
>           let (args,types) = unzip argtypes
>           (argcode, argregs) <- ecomps args
>           let evalcode = map EVAL argregs
>           return $ argcode ++ evalcode ++ [FOREIGN ty reg fn (zip argregs types)]

>     ecomps :: [Expr] -> State CompileState (Bytecode, [TmpVar])
>     ecomps e = ecomps' [] [] e
>     ecomps' code tmps [] = return (code, tmps)
>     ecomps' code tmps (e:es) =
>         do reg <- new_tmp
>            ecode <- ecomp Middle e reg
>            ecomps' (code++ecode) (tmps++[reg]) es

Compile case alternatives.

>     order :: [CaseAlt] -> [CaseAlt]
>     order xs = insertError 0 (sort xs)
>     insertError t [] = []
>     insertError t (a@(Alt tn _ _):xs)
>         = (errors t tn) ++ a:(insertError (tn+1) xs)
>     errors x end | x == end = []
>                  | otherwise = (Alt x [] Impossible):(errors (x+1) end)

>     altcomps :: TailCall -> [CaseAlt] -> TmpVar -> TmpVar -> 
>                 State CompileState [Bytecode]
>     altcomps tc [] _ _ = return []
>     altcomps tc (a:as) scrutinee reg = 
>         do acode <- altcomp tc a scrutinee reg
>            ascode <- altcomps tc as scrutinee reg
>            return (acode:ascode)

Assume that all the tags are in order, and unused constructors have 
a default inserted (i.e., tag can be ignored).

>     altcomp :: TailCall -> CaseAlt -> TmpVar -> TmpVar -> 
>                State CompileState Bytecode
>     altcomp tc (Alt tag nmargs expr) scrutinee reg =
>         do let args = map snd nmargs
>            local <- new_locals (length args)
>            projcode <- project args scrutinee local 0
>            exprcode <- ecomp tc expr reg
>            return (projcode++exprcode)

>     project [] _ _ _ = return []
>     project (_:as) scr loc arg = 
>         do let acode = PROJVAR loc scr arg
>            ascode <- project as scr (loc+1) (arg+1)
>            return (acode:ascode)

Compile an application of a function to arguments

>     acomp :: TailCall -> Bool -> Expr -> [Expr] -> TmpVar -> 
>              State CompileState Bytecode
>     acomp tc lazy (R x) args reg
>           | lazy == False && arity x ctxt == length args =
>               do (argcode, argregs) <- ecomps args
>                  return $ argcode ++ map EVAL argregs ++ [(tcall tc) reg x argregs]
>           | otherwise =
>               do (argcode, argregs) <- ecomps args
>                  return $ argcode ++ [THUNK reg (arity x ctxt) x argregs]
>      where tcall Tail = TAILCALL
>            tcall Middle = CALL
>     acomp _ _ f args reg
>           = do (argcode, argregs) <- ecomps args
>                reg' <- new_tmp
>                fcode <- ecomp Middle f reg'
>                return $ fcode ++ argcode ++ [ADDARGS reg reg' argregs]

>     ccomp (MkInt i) reg = return [INT reg i]
>     ccomp (MkBigInt i) reg = return [BIGINT reg i]
>     ccomp (MkChar c) reg = return [INT reg (fromEnum c)]
>     ccomp (MkFloat f) reg = return [FLOAT reg f]
>     ccomp (MkBigFloat f) reg = return [BIGFLOAT reg f]
>     ccomp (MkBool b) reg = return [INT reg (if b then 1 else 0)]
>     ccomp (MkString s) reg = return [STRING reg s]
>     ccomp (MkUnit) reg = return [UNIT reg]

