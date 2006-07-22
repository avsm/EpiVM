> module EMachine.CodegenC where

> import Control.Monad.State

> import EMachine.Language
> import EMachine.Bytecode

> codegenC :: Context -> [Decl] -> String
> codegenC ctxt decs =
>     fileHeader ++
>     headers decs ++ "\n" ++
>     wrappers decs ++
>     workers ctxt decs ++
>     mainDriver

> fileHeader = "#include \"closure.h\"\n#include <assert.h>\n\n"
> mainDriver = "int main() { GC_init(); _do__U_main(); return 0; }\n"

> showarg _ i = "void* " ++ loc i

> showargs [] i= ""
> showargs [x] i = showarg x i
> showargs (x:xs) i = showarg x i ++ ", " ++ showargs xs (i+1)

> headers [] = ""
> headers ((Decl fname ret (Bind args _ _)):xs) =
>     "void* " ++ thunk fname ++ "(void** block);\n" ++
>     "void* " ++ quickcall fname ++ "(" ++ showargs args 0 ++ ");\n" ++
>     headers xs
> headers ((Include h):xs) = "#include <"++h++">\n" ++ headers xs
> headers (_:xs) = headers xs

> wrappers [] = ""
> wrappers ((Decl fname ret (Bind args _ _)):xs) =
>     "void* " ++ thunk fname ++ "(void** block) {\n    return " ++ 
>     quickcall fname ++ "(" ++
>     wrapperArgs (length args) ++ ");\n}\n\n" ++
>     wrappers xs
> wrappers (_:xs) = wrappers xs

> wrapperArgs 0 = ""
> wrapperArgs 1 = "block[0]"
> wrapperArgs x = wrapperArgs (x-1) ++ ", block[" ++ show (x-1) ++ "]"

> workers _ [] = ""
> workers ctxt ((Decl fname ret func@(Bind args locals defn)):xs) =
>     "void* " ++ quickcall fname ++ "(" ++ showargs args 0 ++ ") {\n" ++
>      compileBody (compile ctxt func) ++ "\n}\n\n" ++
>     workers ctxt xs
> workers ctxt (_:xs) = workers ctxt xs

> tmp v = "tmp" ++ show v
> loc v = "var" ++ show v

> quickcall fn = "_do_" ++ show fn
> thunk fn = "_wrap_" ++ show fn

> compileBody :: FunCode -> String
> compileBody (Code args bytecode) = 
>     let (code, b) = runState (cgs bytecode) 0 in
>         if (b>0) then "void* block["++show b++"];\n"++code else code
>   where
>    sizeneeded x = do
>       max <- get
>       if (x>max) then put x else return ()

>    cgs [] = return ""
>    cgs (x:xs) = do xc <- cg x
>                    xsc <- cgs xs
>                    return $ xc ++ "\n" ++ xsc

>    cg (CALL t fn args) = return $ tmp t ++ " = " ++ quickcall fn ++ 
>                          targs "(" args ++ ");"
>    cg (TAILCALL t fn args) = return $ "return " ++ quickcall fn ++ 
>                          targs "(" args ++ ");"
>    cg (THUNK t ar fn []) = do
>        return $ tmp t ++ 
>           " = (void*)CLOSURE(" ++ thunk fn ++ ", " ++ 
>           show ar ++ ", 0, 0);"
>    cg (THUNK t ar fn args) = do
>        sizeneeded (length args)
>        return $ argblock "block" args ++ tmp t ++ 
>           " = (void*)CLOSURE(" ++ thunk fn ++ ", " ++ 
>           show ar ++ "," ++ show (length args) ++ 
>           ", block);"
>    cg (ADDARGS t th args) = do sizeneeded (length args)
>                                return $ closureApply t th args
>    cg (FOREIGN ty t fn args) = return $ 
>                                castFrom t ty 
>                                   (fn ++ "(" ++ foreignArgs args ++ ")")
>                                   ++ ";"
>    cg (VAR t l) = return $ tmp t ++ " = " ++ loc l ++ ";"
>    cg (ASSIGN l t) = return $ loc l ++ " = " ++ tmp t ++ ";"
>    cg (CON t tag args) = do sizeneeded (length args)
>                             return $ constructor t tag args
>    cg (UNIT t) = return $ tmp t ++ " = MKUNIT;"
>    cg (INT t i) = return $ tmp t ++ " = MKINT("++show i++");"
>    cg (FLOAT t i) = return $ tmp t ++ " = MKFLOAT("++show i++");"
>    cg (STRING t s) = return $ tmp t ++ " = MKSTR("++show s++");"
>    cg (PROJ t1 t2 i) = return $ tmp t1 ++ " = PROJECT((Closure*)"++tmp t2++", "++show i++");"
>    cg (PROJVAR l t i) = return $ loc l ++ " = PROJECT((Closure*)"++tmp t++", "++show i++");"
>    cg (OP t op l r) = return $ doOp t op l r 
>    cg (LOCALS n) = return $ declare "void* " loc (length args) n
>    cg (TMPS n) = return $ declare "void* " tmp 0 n
>    cg (CASE v alts) = do
>        altscode <- cgalts alts 0
>        return $ "assert(ISCON("++tmp v++"));\n" ++
>                   "switch(TAG(" ++ tmp v ++")) {\n" ++
>                   altscode
>                   ++ "}"
>    cg (IF v t e) = do
>        tcode <- cgs t
>        ecode <- cgs e
>        return $ "assert(ISINT("++tmp v++"));\n" ++
>                 "if (GETINT("++tmp v++")) {\n" ++ tcode ++ "} else {\n" ++
>                 ecode ++ "}"
>    cg (EVAL v) = return $ "EVAL((VAL)"++tmp v++");"
>    cg (RETURN t) = return $ "return "++tmp t++";"
>    cg (ERROR s) = return $ "ERROR("++show s++");"
>    -- cg x = return $ "NOP; // not done " ++ show x

>    cgalts [] _ = return $ ""
>    cgalts (bc:alts) tag = do bcode <- cgs bc
>                              altscode <- cgalts alts (tag+1)
>                              return $ "case "++show tag++":\n" ++
>                                     bcode ++ "break;\n" ++ altscode

>    targs st [] = st
>    targs st [x] = st ++ tmp x
>    targs st (x:xs) = st ++ tmp x ++ targs ", " xs

>    argblock name [] = "" -- name ++ " = 0;\n"
>    argblock name args = -- name ++ " = EMALLOC(sizeof(void*)*" ++ show (length args) ++ ");\n" ++ 
>                         ab name args 0
>    ab nm [] i = ""
>    ab nm (x:xs) i = nm ++ "[" ++ show i ++ "] = " ++ tmp x ++";\n" ++ 
>                     ab nm xs (i+1)

>    constructor t tag []
>          = tmp t ++ " = CONSTRUCTOR(" ++
>            show tag ++ ", 0, 0);"
>    constructor t tag args 
>        | length args < 3 && length args > 0
>          = tmp t ++ " = CONSTRUCTOR" ++ show (length args) ++ "(" ++
>            show tag ++ targs ", " args ++ ");"
>    constructor t tag args = argblock "block" args ++ tmp t ++
>                             " = (void*)CONSTRUCTOR(" ++ 
>                             show tag ++ ", " ++ 
>                             show (length args) ++
>                             ", block);"

>    closureApply t th []
>          = tmp t ++ " = CLOSURE_APPLY((VAL)" ++
>            tmp th ++ ", 0, 0);"
>    closureApply t th args 
>        | length args < 3 && length args > 0
>          = tmp t ++ " = CLOSURE_APPLY" ++ show (length args) ++ "((VAL)" ++
>            tmp th ++ targs ", " args ++ ");"
>    closureApply t th args = argblock "block" args ++ tmp t ++ 
>                        " = CLOSURE_APPLY((VAL)" ++ 
>                           tmp th ++ ", " ++ 
>                           show (length args) ++ 
>                           ", block);"

> declare decl fn start end 
>     | start == end = ""
>     | otherwise = decl ++ fn start ++";\n" ++
>                   declare decl fn (start+1) end

> foreignArgs [] = ""
> foreignArgs [x] = foreignArg x
> foreignArgs (x:xs) = foreignArg x ++ ", " ++ foreignArgs xs

> castFrom t TyUnit x = x
> castFrom t TyString rest = tmp t ++ " = MKSTR((char*)(" ++ rest ++ "))"
> castFrom t TyInt rest = tmp t ++ " = MKINT((int)(" ++ rest ++ "))"
> castFrom t _ rest = tmp t ++ " = (void*)(" ++ rest ++ ")"

> foreignArg (t, TyInt) = "GETINT("++ tmp t ++")"
> foreignArg (t, TyString) = "GETSTR("++ tmp t ++")"

> doOp t Plus l r = tmp t ++ " = INTOP(+,"++tmp l ++ ", "++tmp r++");"
> doOp t Minus l r = tmp t ++ " = INTOP(-,"++tmp l ++ ", "++tmp r++");"
> doOp t Times l r = tmp t ++ " = INTOP(*,"++tmp l ++ ", "++tmp r++");"
> doOp t Divide l r = tmp t ++ " = INTOP(/,"++tmp l ++ ", "++tmp r++");"
> doOp t OpEQ l r = tmp t ++ " = INTOP(==,"++tmp l ++ ", "++tmp r++");"
> doOp t OpGT l r = tmp t ++ " = INTOP(>,"++tmp l ++ ", "++tmp r++");"
> doOp t OpLT l r = tmp t ++ " = INTOP(<,"++tmp l ++ ", "++tmp r++");"
> doOp t OpGE l r = tmp t ++ " = INTOP(>=,"++tmp l ++ ", "++tmp r++");"
> doOp t OpLE l r = tmp t ++ " = INTOP(<=,"++tmp l ++ ", "++tmp r++");"

