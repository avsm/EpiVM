> module Epic.Bytecode where

> import Control.Monad.State
> import List

> import Epic.Language
> import Debug.Trace

> type Local = Int
> type TmpVar = Int
> type StrVar = Int

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
>             | TMPASSIGN TmpVar TmpVar
>             | NOASSIGN Local TmpVar -- No-op, but flag not to eval the register
>             | CON TmpVar Tag [TmpVar]
>             | UNIT TmpVar
>             | UNUSED TmpVar
>             | INT TmpVar Int
>             | BIGINT TmpVar Integer
>             | FLOAT TmpVar Float
>             | BIGFLOAT TmpVar Double
>             | STRING TmpVar StrVar
>             | PROJ TmpVar TmpVar Int -- project into a register
>             | PROJVAR Local TmpVar Int -- project into a local variable
>               -- each case branch records which tag it's code for
>             | CASE TmpVar [(Int, Bytecode)] (Maybe Bytecode)
>             | INTCASE TmpVar [(Int, Bytecode)] (Maybe Bytecode)
>             | IF TmpVar Bytecode Bytecode
>             | OP TmpVar Op TmpVar TmpVar
>             | LOCALS Int -- allocate space for locals
>             | TMPS Int -- declare temporary variables
>             | CONSTS [String] -- declare constants
>             | LABEL Int
>             | WHILE Bytecode Bytecode
>             | BREAKFALSE TmpVar
>             | JFALSE TmpVar Int
>             | JUMP Int
>             | EVAL TmpVar Bool -- Bool is True if update required
>             -- | LET TmpVar Local TmpVar
>             | RETURN TmpVar
>             | DRETURN -- return dummy value
>             | ERROR String -- Fatal error, exit
>             | TRACE String [TmpVar]
>             | COMMENT String -- handy for adding notes to output
>   deriving Show

> type Bytecode = [ByteOp]

> data FunCode = Code [Type] Bytecode
>   deriving Show

> data CompileState = CS { arg_types :: [Type],
>                          num_locals :: Int,
>                          next_tmp :: Int,
>                          string_pool :: [String],
>                          max_tmp :: Int,
>                          next_label :: Int }

> compile :: Context -> Name -> Func -> FunCode
> compile ctxt fname fn@(Bind args locals def) = 
>     let cs = (CS (map snd args) (length args) 1 [] 1 0)
>         code = evalState (scompile ctxt fname fn) cs 
>         opt = peephole code in
>               Code (map snd args) opt

> data TailCall = Tail | Middle

> scompile :: Context -> Name -> Func -> State CompileState Bytecode
> scompile ctxt fname (Bind args locals def) = 
>     do -- put (CS args (length args) 1)
>        code <- ecomp (False, True) Tail def 0 (length args)
>        cs <- get
>        return $ (LOCALS (num_locals cs)):
>                 (TRACE (show fname) [0..(length args)-1]):
>                 (TMPS (max_tmp cs)):(CONSTS (string_pool cs)):code ++
>                   [EVAL 0 True, RETURN 0]

>   where

>     new_tmp :: State CompileState Int
>     new_tmp = do cs <- get
>                  let reg' = next_tmp cs
>                  let max = if (reg'+ 1) > max_tmp cs then reg'+1 else max_tmp cs
>                  put (cs { next_tmp = reg'+1, max_tmp = max } )
>                  return reg'

>     set_tmp :: Int -> State CompileState ()
>     set_tmp n = do cs <- get
>                    put (cs { next_tmp = n } )

>     get_tmp :: State CompileState Int
>     get_tmp = do cs <- get
>                  return $ next_tmp cs

>     new_label :: State CompileState Int
>     new_label = do cs <- get
>                    let reg' = next_label cs
>                    put (cs { next_label = reg'+1 } )
>                    return reg'

>     new_string :: String -> State CompileState Int
>     new_string s = do cs <- get
>                       let reg' = string_pool cs
>                       put (cs { string_pool = reg'++[s] } )
>                       return (length reg')

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

>     ecomp :: (Bool, Bool) -> TailCall -> Expr -> TmpVar -> Int -> 
>              State CompileState Bytecode
>     ecomp lazy tcall (V v) reg vs = 
>         do return [VAR reg v]
>     ecomp lazy tcall (R x) reg vs = do
>       savetmp <- get_tmp
>       code <- acomp tcall lazy (R x) [] reg vs
>       set_tmp savetmp
>       return code
>     ecomp lazy tcall (App f as) reg vs = do
>       savetmp <- get_tmp
>       code <- acomp tcall lazy f as reg vs
>       set_tmp savetmp
>       return code
>     ecomp (lazy, update) tcall (Lazy e) reg vs = ecomp (True, update) tcall e reg vs
>     ecomp (lazy, update) tcall (Effect e) reg vs = 
>         do ecode <- ecomp (lazy, False) tcall e reg vs
>            return (ecode ++ [EVAL reg False])
>     ecomp lazy tcall (Con t as) reg vs = 
>         do (argcode, argregs) <- ecomps lazy as vs
>            return $ argcode ++ [CON reg t argregs]
>     ecomp lazy tcall (Proj con i) reg vs =
>         do reg' <- new_tmp
>            concode <- ecomp lazy Middle con reg' vs
>            return $ concode ++ [EVAL reg' (snd lazy), PROJ reg reg' i]
>     ecomp lazy tcall (Const c) reg vs = ccomp c reg
>     ecomp lazy tcall (Case scrutinee alts) reg vs =
>         do screg <- new_tmp
>            sccode <- ecomp lazy Middle scrutinee screg vs
>            (altcode, def) <- altcomps lazy tcall (order alts) screg reg vs
>            return $ sccode ++ [EVAL screg (snd lazy), (caseop alts) screg altcode def]
>     ecomp lazy tcall (If a t e) reg vs =
>         do areg <- new_tmp
>            acode <- ecomp lazy Middle a areg vs
>            tcode <- ecomp lazy tcall t reg vs
>            ecode <- ecomp lazy tcall e reg vs
>            return $ acode ++ [EVAL areg (snd lazy), IF areg tcode ecode]
>     ecomp lazy tcall (While t b) reg vs =
>         do savetmp <- get_tmp
>            start <- new_label
>            end <- new_label
>            treg <- new_tmp
>            tcode <- ecomp lazy Middle t treg vs
>            bcode <- ecomp lazy Middle b reg vs
>            set_tmp savetmp
>            return $ [WHILE (tcode++[EVAL treg False, BREAKFALSE treg]) bcode]

(LABEL start):tcode ++ 
                     (EVAL treg False):(JFALSE treg end):bcode ++
                     [EVAL reg False, JUMP start, LABEL end]

>     ecomp lazy tcall (Op op l r) reg vs =
>         do savetmp <- get_tmp
>            lreg <- new_tmp
>            rreg <- new_tmp
>            lcode <- ecomp lazy Middle l lreg vs
>            rcode <- ecomp lazy Middle r rreg vs
>            set_tmp savetmp
>            return $ lcode ++ [EVAL lreg (snd lazy)] ++ 
>                     rcode ++ [EVAL rreg (snd lazy), OP reg op lreg rreg]
>     ecomp lazy tcall (Let nm ty val scope) reg vs =
>         do loc <- new_locals 1
>            reg' <- new_tmp
>            valcode <- ecomp lazy Middle val reg' vs
>            scopecode <- ecomp lazy tcall scope reg (vs+1)
>            let assigncode = case ty of
>                               TyUnit -> [ASSIGN vs reg']
>                               _ -> [ASSIGN vs reg']
>            return $ valcode ++ assigncode ++ scopecode
>     ecomp lazy tcall (Error str) reg vs = return [ERROR str]
>     ecomp lazy tcall Impossible reg vs = return [ERROR "The impossible happened."]
>     ecomp lazy tcall (ForeignCall ty fn argtypes) reg vs = do
>           savetmp <- get_tmp
>           let (args,types) = unzip argtypes
>           (argcode, argregs) <- ecompsEv lazy args vs
>           -- let evalcode = if (snd lazy) then [] else map (\x -> EVAL x (snd lazy)) argregs
>           set_tmp savetmp
>           return $ argcode ++ [FOREIGN ty reg fn (zip argregs types)]
>     ecomp lazy tcall (LazyForeignCall ty fn argtypes) reg vs = do
>           savetmp <- get_tmp
>           let (args,types) = unzip argtypes
>           (argcode, argregs) <- ecomps lazy args vs
>           set_tmp savetmp
>           return $ argcode ++ [FOREIGN ty reg fn (zip argregs types)]

>     ecomps :: (Bool, Bool) -> [Expr] -> Int -> State CompileState (Bytecode, [TmpVar])
>     ecomps lazy e vs = ecomps' lazy [] [] e vs
>     ecomps' lazy code tmps [] vs = return (code, tmps)
>     ecomps' lazy code tmps (e:es) vs =
>         do reg <- new_tmp
>            ecode <- ecomp lazy Middle e reg vs
>            ecomps' lazy (code++ecode) (tmps++[reg]) es vs

>     ecompsEv :: (Bool, Bool) -> [Expr] -> Int -> State CompileState (Bytecode, [TmpVar])
>     ecompsEv lazy e vs = ecompsEv' lazy [] [] e vs
>     ecompsEv' lazy code tmps [] vs = return (code, tmps)
>     ecompsEv' lazy code tmps (e:es) vs =
>         do reg <- new_tmp
>            ecode <- ecomp lazy Middle e reg vs
>            let evcode = if (snd lazy) then [] else [EVAL reg (snd lazy)]
>            ecompsEv' lazy (code++ecode++evcode) (tmps++[reg]) es vs

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

>     altcomps :: (Bool, Bool) -> TailCall -> [CaseAlt] -> TmpVar -> TmpVar -> Int ->
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

>     altcomp :: (Bool, Bool) -> TailCall -> CaseAlt -> TmpVar -> TmpVar -> Int ->
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

>     acomp :: TailCall -> (Bool, Bool) -> Expr -> [Expr] -> TmpVar -> Int ->
>              State CompileState Bytecode
>     acomp tc lazy (R x) args reg vs
>           | fst lazy == False && arity x ctxt == length args =
>               do (argcode, argregs) <- ecomps lazy args vs
>                  return $ argcode {- ++ map EVAL argregs -} ++ [(tcall tc) reg x argregs]
>           | otherwise =
>               do (argcode, argregs) <- ecomps lazy args vs
>                  return $ argcode ++ [THUNK reg (arity x ctxt) x argregs] ++ 
>                           if (not (fst lazy)) then [EVAL reg (snd lazy)] else []
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
>     ccomp (MkString s) reg = do sreg <- new_string s
>                                 return [STRING reg sreg]
>     ccomp (MkUnit) reg = return [UNIT reg]
>     ccomp MkUnused reg = return [UNUSED reg]



> peephole :: Bytecode -> Bytecode
> peephole = peephole' []

> peephole' ev [] = []
> peephole' ev ((CASE t cases mcs):cs)
>    = CASE t (map (\ (x,c) -> (x, peephole' ev c)) cases) (fmap (peephole' ev) mcs) : peephole' ev cs
> peephole' ev ((INTCASE t cases mcs):cs)
>    = INTCASE t (map (\ (x,c) -> (x, peephole' ev c)) cases) (fmap (peephole' ev) mcs) : peephole' ev cs
> peephole' ev (c: ASSIGN v1 r1: VAR r2 v2: EVAL r3 b: cs)
>    | v1 == v2 && r3 == r2 = peephole' ev (c : EVAL r1 b: ASSIGN v1 r1 : TMPASSIGN r3 r1 : cs)
> peephole' ev ((IF v t e):cs) = IF v (peephole' ev t) (peephole' ev e) : peephole' ev cs
> peephole' ev ((WHILE t b):cs) = WHILE (peephole' ev t) (peephole' ev b) : peephole' ev cs
> peephole' ev (EVAL v l: EVAL v' l':cs) 
>              | v == v' && l == l' = peephole' ev ((EVAL v l):cs)
> peephole' ev (c:EVAL v l:xs) | evalled ev v c 
>                                  = peephole' ev (c:xs)
>                              | otherwise = c:peephole' ev (EVAL v l: xs)
> peephole' ev (c:ASSIGN v r:cs)
>              | evalled [] r c = c:ASSIGN v r:peephole' (v:ev) cs
>              | otherwise = c: peephole' ev (ASSIGN v r: cs)
> peephole' ev (x:xs) = x:peephole' ev xs

> evalled ev v (INT x i) = x==v
> evalled ev v (OP x _ _ _) = x==v
> evalled ev v (CON x _ _) = x==v
> evalled ev v (STRING x _) = x==v
> evalled ev v (CALL x _ _) = x==v -- functions always eval before return
> evalled ev v (FOREIGN _ x _ _) = x==v
> evalled ev v (VAR x l) = x==v && l `elem` ev
> evalled ev v (UNIT x) = x==v
> evalled ev v (UNUSED x) = x==v
> evalled _ _ _ = False
