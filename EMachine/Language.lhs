> module EMachine.Language where

> import Control.Monad

Raw data types. Int, Char, Bool are unboxed.

> data Type = TyInt
>           | TyChar
>           | TyBool
>           | TyFloat
>           | TyString
>           | TyUnit
>           | TyAny -- unchecked, polymorphic
>           | TyData -- generic data type
>           | TyFun -- any function
>   deriving (Show, Eq)

> data Const = MkInt Int
>            | MkChar Char
>            | MkFloat Float
>            | MkString String
>            | MkBool Bool
>            | MkUnit
>   deriving Show

> data Name = UN String  -- user name
>           | MN String Int -- machine generated name
>   deriving Eq

> instance Show Name where
>     show (UN str) = "_U_"++str
>     show (MN str i) = "_M_"++show i++"_"++str

> showuser (UN str) = str
> showuser (MN str i) = "["++str++"_"++show i++"]"

> type Context = [(Name,([Type],Type))] -- Name, arg types, return type

Get the arity of a definition in the context

> arity x ctxt = case lookup x ctxt of
>                   Nothing -> error $ "No such function " ++ show x
>                   (Just (args,ret)) -> length args

> data Expr = V Int -- Locally bound name
>           | R Name -- Global reference
>           | App Expr [Expr] -- Function application
>           | LazyApp Expr [Expr] -- Lazy function application
>           | Con Int [Expr] -- Constructor, tags, arguments (fully applied)
>           | Const Const -- a constant
>           | Proj Expr Int -- Project argument, say what type it is
>           | Case Expr [CaseAlt]
>           | If Expr Expr Expr
>           | Op Op Expr Expr -- Infix operator
>           | Let Name Type Expr Expr -- Let binding
>           | Error String -- Exit with error message
>           | Impossible -- Claimed impossible to reach code
>           | ForeignCall Type String [(Expr, Type)] -- Foreign function call
>   deriving Show

> data CaseAlt = Alt { alt_tag :: Int,
>                      alt_args :: [(Name, Type)], -- bound arguments
>                      alt_expr :: Expr -- what to do
>                    }
>   deriving Show

> data Op = Plus | Minus | Times | Divide | OpEQ | OpLT | OpLE | OpGT | OpGE
>   deriving Show

Supercombinator definitions

> data Func = Bind { fun_args :: [(Name, Type)],
>                    locals :: Int, -- total number of locals
>                    defn :: Expr }
>   deriving Show

Programs

> data Decl = Decl { fname :: Name,
>                    frettype :: Type,
>                    fdef :: Func }
>           | Include String
>           | Link String
>   deriving Show

> data Result r = Success r
>               | Failure String String Int
>     deriving (Show, Eq)
> 
> instance Monad Result where
>     (Success r)   >>= k = k r
>     (Failure err fn line) >>= k = Failure err fn line
>     return              = Success
>     fail s              = Failure s "(no file)" 0
> 
> instance MonadPlus Result where
>     mzero = Failure "Error" "(no file)" 0
>     mplus (Success x) _ = (Success x)
>     mplus (Failure _ _ _) y = y
> 



Some tests


foo x = let y = case x of
          c1 a b -> a b
          c2 c -> bar (c+2)
            in y+3




 testctxt = [((UN "foo"),([TyData], TyInt)),
            ((UN "bar"),([TyInt], TyInt))]

 testprog = Bind [TyData] 3 $
               Let (UN "y") TyInt (Case (V 0)
                 [Alt 0 [TyFun,TyInt] (App (V 1) [V 2]),
                  Alt 1 [TyInt] (App (R (UN "bar")) [Op Plus (V 1) (Const (MkInt 2))])])
               (Op Plus (V 1) (Const (MkInt 3)))

