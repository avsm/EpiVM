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

>         simplFun (Bind args locs def) 
>             = Bind args locs (simplify sctxt (map (\x -> Nothing) args) (length args) def)
>         diff fn simpled fd x | defn simpled == defn fd = x
>                              | otherwise = {- trace (show fn ++ "\n" ++ show simpled ++ "\n" ++
>                                                   show fd) -} x

> inlinable = elem Inline

> simplify :: SCtxt -> [Maybe Expr] -> Int -> Expr -> Expr
> simplify sctxt args arity exp = s' args exp where
>     s' args (V i) = if i<length args then 
>                         case args!!i of
>                             Nothing -> V i
>                             Just v -> v
>                         else V (i + (arity - length args)) -- adjust case/let offset
>     s' args (R fn) 
>       = case lookup fn sctxt of
>             Just (decl, 0, fl) -> 
>                if (inlinable fl) then s' args (inline decl [])
>                    else R fn
>             _ -> R fn
>     s' args (App f a) = apply (s' args f) (map (s' args) a)
>     s' args (Lazy e) = Lazy $ s' args e
>     s' args (Effect e) = Effect $ s' args e
>     s' args (Con t a) = Con t (map (s' args) a)
>     s' args (Proj e i) = project (s' args e) i
>     s' args (Case e alts) = runCase (s' args e) (map (salt args) alts)
>     s' args (If x t e) = runIf (s' args x) (s' args t) (s' args e)
>     s' args (Op op l r) = runOp op (s' args l) (s' args r)
>     s' args (Let n ty v sc) = Let n ty (s' args v) (s' args sc)
>     s' args (ForeignCall ty nm a) 
>           = ForeignCall ty nm (map (\ (x,y) -> (s' args x, y)) a)
>     s' args (LazyForeignCall ty nm a) 
>           = LazyForeignCall ty nm (map (\ (x,y) -> (s' args x, y)) a)
>     s' args x = x

>     salt args (Alt t bargs e) = Alt t bargs (s' args e)
>     salt args (ConstAlt c e) = ConstAlt c (s' args e)
>     salt args (DefaultCase e) = DefaultCase (s' args e)

>     project e i = Proj e i
>     runCase e alts = Case e alts
>     runIf x t e = If x t e
>     runOp op l r = Op op l r

>     apply f@(R fn) as 
>       = case lookup fn sctxt of
>             Just (decl, ar, fl) -> 
>                if (inlinable fl && ar == length as) then s' args (inline decl as)
>                    else App f as
>             _ -> App f as
>     apply f as = App f as

>     inline :: Decl -> [Expr] -> Expr
>     inline (Decl _ _ (Bind _ _ exp) _ _) args = simplify sctxt (map Just args) arity exp
