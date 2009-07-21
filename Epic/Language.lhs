> module Epic.Language where

> import Control.Monad

Raw data types. Int, Char, Bool are unboxed.

> data Type = TyInt
>           | TyChar
>           | TyBool
>           | TyFloat
>           | TyBigInt
>           | TyBigFloat
>           | TyString
>           | TyPtr
>           | TyUnit
>           | TyAny -- unchecked, polymorphic
>           | TyData -- generic data type
>           | TyFun -- any function
>   deriving Eq

> instance Show Type where
>     show TyInt = "Int"
>     show TyChar = "Char"
>     show TyBool = "Bool"
>     show TyFloat = "Float"
>     show TyBigInt = "BigInt"
>     show TyBigFloat = "BigFloat"
>     show TyString = "String"
>     show TyPtr = "Ptr"
>     show TyUnit = "Unit"
>     show TyAny = "Any"
>     show TyData = "Data"
>     show TyFun = "Fun"

> data Const = MkInt Int
>            | MkBigInt Integer
>            | MkChar Char
>            | MkFloat Float
>            | MkBigFloat Double
>            | MkString String
>            | MkBool Bool
>            | MkUnit
>   deriving (Show, Eq)

> data Name = UN String  -- user name
>           | MN String Int -- machine generated name
>   deriving Eq

> instance Show Name where
>     show (UN str) = "_U_"++str
>     show (MN str i) = "_M_"++show i++"_"++str

> showuser (UN str) = str
> showuser (MN str i) = "["++str++"_"++show i++"]"

> quotename [] = ""
> quotename ('_':cs) = "__"++quotename cs
> quotename ('\'':cs) = "_PR_"++quotename cs
> quotename ('?':cs) = "_QU_"++quotename cs
> quotename ('$':cs) = "_DO_"++quotename cs
> quotename ('#':cs) = "_HA_"++quotename cs
> quotename ('@':cs) = "_AT_"++quotename cs
> quotename (c:cs) = c:(quotename cs)

> showC n = quotename (show n)

> type Context = [(Name,([Type],Type))] -- Name, arg types, return type

Get the arity of a definition in the context

> arity x ctxt = case lookup x ctxt of
>                   Nothing -> error $ "No such function " ++ show x
>                   (Just (args,ret)) -> length args

> type Tag = Int

> data Expr = V Int -- Locally bound name
>           | R Name -- Global reference
>           | App Expr [Expr] -- Function application
>           | Lazy Expr -- Lazy function application
>           | Con Tag [Expr] -- Constructor, tags, arguments (fully applied)
>           | Const Const -- a constant
>           | Proj Expr Int -- Project argument
>           | Case Expr [CaseAlt]
>           | If Expr Expr Expr
>           | Op Op Expr Expr -- Infix operator
>           | Let Name Type Expr Expr -- Let binding
>           | Error String -- Exit with error message
>           | Impossible -- Claimed impossible to reach code
>           | ForeignCall Type String [(Expr, Type)] -- Foreign function call
>           | LazyForeignCall Type String [(Expr, Type)] -- Foreign function call
>   deriving (Show, Eq)

> data CaseAlt = Alt { alt_tag :: Tag,
>                      alt_args :: [(Name, Type)], -- bound arguments
>                      alt_expr :: Expr -- what to do
>                    }
>              | ConstAlt { alt_const :: Int,
>                           alt_expr :: Expr }
>              | DefaultCase { alt_expr :: Expr }
>   deriving (Show, Eq)

> instance Ord CaseAlt where -- only the tag matters
>    compare (Alt t1 _ _) (Alt t2 _ _) = compare t1 t2
>    compare (Alt _ _ _) (DefaultCase _) = LT
>    compare (DefaultCase _) (Alt _ _ _) = GT
>    compare _ _ = EQ

> data Op = Plus | Minus | Times | Divide | OpEQ | OpLT | OpLE | OpGT | OpGE
>   deriving (Show, Eq)

Supercombinator definitions

> data Func = Bind { fun_args :: [(Name, Type)],
>                    locals :: Int, -- total number of locals
>                    defn :: Expr }
>   deriving Show

Programs

> data Decl = Decl { fname :: Name,
>                    frettype :: Type,
>                    fdef :: Func }
>           | Extern { fname :: Name, 
>                      frettype :: Type,
>                      fargs :: [Type] }
>           | Include String
>           | Link String
>           | Export { cname :: Name,
>                      fname :: Name,
>                      export_args :: [(Name, Name)] }
>           | CType Name
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

