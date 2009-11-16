> module Epic.Simplify(simplifyAll) where

> import Epic.Language

> import Data.Maybe
> import Debug.Trace

> simplifyAll :: (Context, [Decl]) -> (Context, [Decl])
> simplifyAll (ctxt, xs) = let sctxt = mapMaybe mkEntry xs in
>                              simpl sctxt ctxt xs
>   where mkEntry d@(Decl n _ fn _ fl) = Just (n, (d, (length (fun_args fn)), fl))
>         mkEntry _ = Nothing

For each supercombinator, evaluate it as far as we believe sensible - basically just inlining
definitions marked as such, constant folding, case on constants, etc.

Also consider creating specialised versions of functions?

> type SCtxt = [(Name, (Decl, Int, [CGFlag]))]

> simpl :: SCtxt -> Context -> [Decl] -> (Context, [Decl])
> simpl sctxt ctxt ds = (ctxt, map simplD ds)
>   where simplD (Decl fn fr fd fe fl) = let simpled = simplFun fd in
>                                        diff fn simpled fd $ 
>                                          Decl fn fr (simplFun fd) fe fl
>         simplD d = d

>         simplFun (Bind args locs def fl) 
>             = Bind args locs (simplify sctxt (map (\x -> Nothing) args) (length args) def) fl
>         diff fn simpled fd x | defn simpled == defn fd = x
>                              | otherwise = {- trace (show fn ++ "\n" ++ show simpled ++ "\n" ++
>                                                   show fd) -} x

> inlinable = elem Inline

> simplify :: SCtxt -> [Maybe Expr] -> Int -> Expr -> Expr
> simplify sctxt args arity exp = exp -- s' args arity exp 
>   where
>     s' args depth (V i) = if i<length args then 
>                             case args!!i of
>                               Nothing -> V i
>                               Just v -> v
>                             else V (i + (arity - length args)) -- adjust case/let offset
>     s' args d (R fn) 
>       = case lookup fn sctxt of
>             Just (decl, 0, fl) -> 
>                if (inlinable fl) then s' args d (inline d decl [])
>                    else R fn
>             _ -> R fn
>     s' args d (App f a) = apply d (s' args d f) (map (s' args d) a)
>     s' args d (Lazy e) = Lazy $ s' args d e
>     s' args d (Effect e) = Effect $ s' args d e
>     s' args d (While t e) = While (s' args d t) (s' args d e)
>     s' args d (WhileAcc t a e) = WhileAcc (s' args d t) (s' args d a) (s' args d e)
>     s' args d (Con t a) = Con t (map (s' args d) a)
>     s' args d (Proj e i) = project (s' args d e) i
>     s' args d (Case e alts) = runCase (s' args d e) (map (salt args d) alts)
>     s' args d (If x t e) = runIf (s' args d x) (s' args d t) (s' args d e)
>     s' args d (Op op l r) = runOp op (s' args d l) (s' args d r)
>     s' args d (Let n ty v sc) = simplFLet $ Let n ty (s' args d v) (s' args (d+1) sc)
>     s' args d (ForeignCall ty nm a) 
>           = ForeignCall ty nm (map (\ (x,y) -> (s' args d x, y)) a)
>     s' args d (LazyForeignCall ty nm a) 
>           = LazyForeignCall ty nm (map (\ (x,y) -> (s' args d x, y)) a)
>     s' args d x = x

>     salt args d (Alt t bargs e) = Alt t bargs (s' args (d+length bargs) e)
>     salt args d (ConstAlt c e) = ConstAlt c (s' args d e)
>     salt args d (DefaultCase e) = DefaultCase (s' args d e)

>     project e i = Proj e i
>     runCase e alts = Case e alts
>     runIf x t e = If x t e
>     runOp op l r = Op op l r

>     apply d f@(R fn) as 
>       = case lookup fn sctxt of
>             Just (decl, ar, fl) -> 
>                if (inlinable fl && ar == length as) then s' args d (inline d decl as)
>                    else App f as
>             _ -> App f as
>     apply d f as = App f as

>     inline :: Int -> Decl -> [Expr] -> Expr
>     inline d (Decl _ _ (Bind _ _ exp _) _ _) args = simplify sctxt (map Just args) d exp

If we do this, we can chop out some pointless assignments to Unit

> simplFLet :: Expr -> Expr
> simplFLet (Let n _ (ForeignCall ty f args) s) = 
>                  Let n ty (ForeignCall ty f args) s
> simplFLet (Let n _ (Effect (ForeignCall ty f args)) s) =
>                  Let n ty (Effect (ForeignCall ty f args)) s
> simplFLet x = x
